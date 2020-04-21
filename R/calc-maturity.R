calc_maturity <- function(d, type = "length"){
  m_mat <- gfplot::fit_mat_ogive(d, type = type)
  mat_perc <- pbs2dlm:::extract_maturity_perc(coef(m_mat$model))
  mat_perc$se_l50 <- pbs2dlm:::delta_method(~ -(log((1 / 0.5) - 1) + x1 + x3) / (x2 + x4),
                                            mean = coef(m_mat$model), cov = vcov(m_mat$model))
  mat_perc$se_l50_95 <- pbs2dlm:::delta_method(~ -(log((1/0.95) - 1) + x1 + x3) / (x2 + x4) -
                                                 -(log((1/0.5) - 1) + x1 + x3) / (x2 + x4),
                                               mean = coef(m_mat$model), cov = vcov(m_mat$model))
  #gfplot::plot_mat_ogive(m_mat)
  mat_perc
}
