#' Run a VonB model on the given data
#'
#' @param d_samples Sample dat from gfdata, either `survey_samples` or `commercial_samples`
#' @param sex See [gfplot::fir_vb()]
#'
#' @return A table of output values for the length model
#' @export
calc_vb <- function(d_samples, sex = "all"){
  vbm <- gfplot::fit_vb(d_samples, sex = sex)
  vbm_vals <- summary(TMB::sdreport(vbm$model)) %>%
    as.data.frame() %>%
    rownames_to_column(var = "param_name") %>%
    as_tibble() %>%
    transmute(param_name,
              se = `Std. Error`,
              estimate = Estimate,
              cv = se / abs(estimate),
              cv_lower = Estimate - cv,
              cv_upper = Estimate + cv)
  #gfplot::plot_vb(object_all = vbm, col = c("All" = "black"))
  vbm_vals
}

#' Get the lower and upper CV values for a parameter of the length model
#'
#' @param vbm Output of [calc_vb()]
#' @param param_name Name of the parameter as found in the `param_name` column of `vbm`
#'
#' @return A two-element vector of the lower and upper CVs of the parameter
#' @export
get_cvs <- function(vbm, param_name = NULL){
  stopifnot(!is.null(param_name))
  vbm %>%
    filter(param_name == param_name) %>%
    select(cv_lower, cv_upper) %>%
    unlist(., use.names=FALSE)
}