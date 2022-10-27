#' Calculate selectivity parameters from SS values to
#' iscam values
#'
#' @details
#' The ISCAM and SS platforms have slightly different equations for selectivity
#' and that means their parameters are slightly different.l This function takes
#' in parameters from SS (control.ss_new) which are MLE estimates of
#' selectivity and finds the values of the ISCAM parameters for input into the
#' ISCAM control file.
#'
#' @param age_infl Parameter 1 from the SS selectivity
#' @param age_95 Parameter 2 from the SS selectivity
#' @param min_age Minimum age to use in calculation
#' @param max_age Maximum age to use in calculation
#'
#' @return A vector of two (the age-at=50% and sd-at-50% values
#' for input into an iscam control file)
calc_sel_for_iscam <- function(age_infl = 9,
                               age_95 = 2,
                               min_age = 0,
                               max_age = 25){

  age <- seq(min_age, max_age, length.out = 200)
  dat <- 1 / (1 + exp(- log(19) * (age - age_infl) / age_95))

  mdl <- function(par){
    age50e <- plogis(age, par[1], par[2])
    sum((age50e - dat) ^ 2)
  }

  conv <- optim(
    par = c(age_infl, age_95), # starting values
    fn = mdl,
    method = "L-BFGS-B",
    lower = 0.01,
    upper = Inf
  )
  if(conv$convergence != 0){
    stop("Age-at-50% model failed to converge. ",
         "Message from optim was: ", conv$message,
         call. = FALSE)
  }
  conv
}

#' Call [calc_sel_for_iscam()] for a list of gears
#'
#' @return A [tibble::tibble()] containing the gear names and values for the
#' age-at=50% and sd-at-50% parameterrs for each gear
conv_sels <- function(){
  # These are the selectivity estimates that came from the SS MLE output
  sels <- list(c(8.14075, 3.41812),
               c(8.47371, 3.23188),
               c(5.05358, 4.99873),
               c(8.46368, 5.85785),
               c(8.23356, 4.51129))

  gear <- c("FT",
            "SS",
            "QCS Syn",
            "HS Syn",
            "WCVI Syn")

  map(sels, ~{
    calc_sel_for_iscam(.x[1], .x[2])$par |>
      vec2df(nms = c("Age-at-50%", "SD-at-50%"))
  }) |>
    bind_rows() |>
    mutate(Gear = gear) |>
    select(Gear, everything())
}

