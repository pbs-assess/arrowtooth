#' Plot fixed effect coefficients in the model run by [run_discard_index_model()]
#'
#' @param object A model as returned by [run_discard_index_model()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_fixed_effect_coefs <- function(object){

  su <- summary(object)$coefficients$cond
  sud <- as.data.frame(su)
  sud$param <- row.names(su)
  row.names(sud) <- NULL
  sud <- sud |>
    rename(est = Estimate,
           se = `Std. Error`) |>
    mutate(par_value = gsub("^[A-Z_a-z]+", "", param)) |>
    mutate(par_group = gsub("^([A-Z_a-z]+)[0-9.]+$", "\\1", param))

  if(fr()){
    sud <- sud |>
      mutate(par_group = ifelse(par_group == "depth",
                                "profondeur",
                                ifelse(par_group == "latitude",
                                       "latitude",
                                       ifelse(par_group == "month",
                                              "mois",
                                              ifelse(par_group == "year_factor",
                                                     "facteur_année",
                                                     par_group)))))
  }
  sud |> ggplot(aes_string("est", "forcats::fct_rev(par_value)",
                           yend = "forcats::fct_rev(par_value)")) +
    ggplot2::geom_segment(aes_string(x = "est - 1.96 * se",
                                     xend = "est + 1.96 * se"),
                          lwd = 0.5) +
    ggplot2::geom_segment(aes_string(x = "est - 0.67 * se",
                                     xend = "est + 0.67 * se"),
                          lwd = 1.25) +
    geom_point() +
    facet_wrap(~par_group,
               scales = "free") +
    theme_pbs() +
    guides(shape = "none",
                         colour = "none") +
    labs(x = ifelse(fr(),
                    "Valeur du coefficient (espace logarithmique)",
                    "Coefficient value (log space)"),
         y = ifelse(fr(),
                    "Valeur prédictive",
                    "Predictor value"))
}
