#' Plot the design-based vs geostatistical survey indices
#'
#' @param surv_index The data frame returned by [gfdata::get_survey_index()]
#' @param surv_geostat_dr The directory name where the geostatistical model
#' output RDS files reside e.g. 'i-arrowtooth-flounder-16-no-depth.rds'. The
#' RDS files themselves must begin with 'i-arrow'
#'
#' @returns A [ggplot2::ggplot()] object
#' @export
geostat_plot_individual_vs_design <- \(
  surv_index,
  surv_geostat_dr = "/srv/arrowtooth/arrowtooth-nongit/survey-geostat"){

  d <- surv_index |>
    filter(survey_abbrev %in%  c("SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG"))

  fns <- list.files(surv_geostat_dr,
                    full.names = TRUE,
                    pattern = "^i-arrow")
  ind_geo <- purrr::map_dfr(fns, readRDS)

  for_model <- ind_geo |>
    filter(model == "with-depth") |>
    mutate(cv = sqrt(exp(se ^ 2) - 1)) %>%
    select(year, survey, est, lwr, upr, se, log_est, cv) |>
    mutate(weight = 1 / cv) |>
    mutate(bio = round(est / 1e6, 2), weight = round(weight, 2)) |>
    select(survey, year, bio, weight)

  j <- ind_geo |>
    filter(model == "with-depth") |>
    mutate(model = ifelse(fr(),
                          "Géostatistique (avec profondeur)",
                          "Geostatistical (with depth)"))
  k <- d |>
    select(year, lwr = lowerci, upr = upperci, est = biomass, survey = survey_abbrev) |>
    mutate(model = ifelse(fr(),
                          "Basé sur la conception",
                          "Design-based"))
  d_toplot <- bind_rows(j, k)

  g <- d_toplot |>
    ggplot(aes(year, est / 1000, colour = model, fill = model)) +
    geom_ribbon(aes(ymin = lwr / 1000, ymax = upr / 1000),
                alpha = 0.2, colour = NA) +
    geom_line(alpha = 1, lty = 1, lwd = 0.5) +
    geom_point(pch = 21, fill = NA) +
    ylab(ifelse(fr(),
                "Biomasse (tonnes)",
                "Biomass (tonnes)")) +
    coord_cartesian(expand = FALSE) +
    theme_pbs() +
    theme(axis.title.x = element_blank()) +
    facet_wrap(~survey, scales = "free_y") +
    labs(fill = "Type", colour = "Type") +
    scale_fill_brewer(palette = "Set2") +
    scale_colour_brewer(palette = "Set2")

  g
}