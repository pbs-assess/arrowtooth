#' Create a plot of random intercepts for the GLMM discard CPUE index model
#'
#' @param object A model as returned by [run_discard_index_model()]
#' @param re_names A vector of names of GLMM model parameters to include
#' in the plots
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_random_intercepts <- function(object,
                                   re_names = c("locality")){

  re <- make_re_dat(object) |>
    filter(par_group %in% re_names)

  if(fr()){
    re <- re |>
      mutate(par_group =
        ifelse(par_group == "depth",
               "profondeur",
               ifelse(par_group == "latitude",
                      "latitude",
                      ifelse(par_group == "month",
                             "mois",
                             ifelse(par_group == "year_factor",
                                    "facteur_année",
                                    ifelse(par_group == "locality",
                                           "localité",
                                           ifelse(par_group == "vessel",
                                                  "navire",
                                                  par_group)))))))
  }

  re |>
    ggplot(aes_string("est",
                      "forcats::fct_rev(par_value)")) +
    geom_vline(xintercept = 0,
               lty = 2,
               alpha = 0.4) +
    geom_point(bg = "white") +
    facet_wrap(~par_group,
               scales = "free") +
    theme_pbs() +
    guides(shape = "none",
           colour = "none") +
    labs(x = ifelse(fr(),
                    "Valeur de l'ordonnée à l'origine aléatoire (espace logarithmique)",
                    "Random intercept value (log space)"),
         y = "")
}
