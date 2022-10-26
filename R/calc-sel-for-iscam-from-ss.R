calc_sel_for_iscam <- function(a50 = 9, sd50 = 2){

  mdl <- function(par, age50, sd50){
    age50_e <- 1 / (1 + exp(- log(19) * (age50 - par[1]) / par[2]))
    sum((age50_e - age50) ^ 2)
  }

  conv <- optim(par = c(a50, sd50),
                fn = mdl,
                method = "L-BFGS-B",
                lower = 0,
                upper = Inf,
                age50 = a50,
                sd50 = sd50)
  if(conv$convergence != 0){
    stop("Age-at-50% model failed to converge. ",
         "Message from optim was: ", conv$message,
         call. = FALSE)
  }
  conv
}