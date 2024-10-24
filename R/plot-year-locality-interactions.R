#' Plot year-locality interactions for the GLMM model for Discard CPUE
#'
#' @param object A model as returned by [run_discard_index_model()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_year_locality_interactions <- function(object) {

  re <- make_re_dat(object) |>
    filter(par_group == "year_locality")

  re |>
    ggplot(aes_string("as.numeric(loc_year)",
                      "est",
                      group = "loc_group")) +
    geom_hline(yintercept = 0,
               lty = 2,
               alpha = 0.4) +
    geom_point(alpha = 0.7) +
    geom_line(alpha = 0.3) +
    facet_wrap(~loc_group) +
    scale_x_continuous(breaks = seq(1900, 3000, 10)) +
    theme_pbs() +
    guides(shape = "none", colour = "none") +
    labs(x = tr("Year"),
         y = ifelse(fr(),
                    "Valeur de l'intercept aléatoire\n(interaction) (espace logarithmique)",
                    "Random intercept\n(interaction) value (log space)"))
}
