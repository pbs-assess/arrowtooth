#' Calculate the maturity ogive
#'
#' @param d A [data.frame] with the structure of that found in the `survey_sets`
#' object of the [gfdata] package
#' @param type See [gfplot::fit_mat_ogive()]
#'
#' @return A list of several values including length at 5%, 50%, and 95% maturity
#' for males and females, and standard error on length at 50% and 95%
#' @importFrom gfplot fit_mat_ogive
#' @export
calc_maturity <- function(d, type = "length"){
  m_mat <- fit_mat_ogive(d, type = type)
  mat_perc <- gfdlm:::extract_maturity_perc(coef(m_mat$model))
  mat_perc$se_l50 <- gfdlm:::delta_method(~ -(log((1 / 0.5) - 1) + x1 + x3) / (x2 + x4),
                                          mean = coef(m_mat$model), cov = vcov(m_mat$model))
  mat_perc$se_l50_95 <- gfdlm:::delta_method(~ -(log((1/0.95) - 1) + x1 + x3) / (x2 + x4) -
                                               -(log((1/0.5) - 1) + x1 + x3) / (x2 + x4),
                                             mean = coef(m_mat$model), cov = vcov(m_mat$model))
  #gfplot::plot_mat_ogive(m_mat)
  mat_perc
}
