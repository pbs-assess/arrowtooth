#' Plot the condition factor fits by survey
#'
#' @param fit_lst A list of a fit from [sdmTMB] and a data frame
#' of the data used in that fit as returned by [condition_fit()]
#' @param grid_fn The Synoptic grid data frame
#'
#' @returns A [ggplot2::ggplot()] object
#' @export
condition_factor_by_survey <- \(
  fit_lst,
  grid_fn = "/srv/arrowtooth/arrowtooth-nongit/data/synoptic_grid.rds"){

  if(!file.exists(grid_fn)){
    bail("File `", grid_fn, "` does not exist.")
  }

  nd <- readRDS(grid_fn)

  fit <- fit_lst$fit
  ds <- fit_lst$ds

  fitted_yrs <- sort(unique(ds$year))
  nd <- make_grid(nd, years = fitted_yrs) |>
    na.omit() |>
    mutate(year = as.integer(year)) |>
    mutate(log_depth = log(depth))

  out <- split(nd, nd$survey) |>
    map(~{
      p <- predict(fit, newdata = .x, nsim = 300)
      ind <- get_index_sims(p,
                            agg_function = \(x) mean(x),
                            area_function = \(x, area) x * area)
      ind})
  outs <- bind_rows(out, .id = "survey")

  g <- ggplot(outs,
              aes(year,
                  exp(est),
                  ymin = exp(lwr),
                  ymax = exp(upr),
                  colour = survey,
                  fill = survey)) +
    geom_ribbon(alpha= 0.5, colour = NA) +
    geom_line() +
    scale_fill_brewer(palette = "Set2") +
    scale_colour_brewer(palette = "Set2") +
    theme_pbs() +
    ylab(ifelse(fr(),
                "Facteur de condition",
                "Condition factor")) +
    xlab(tr("Year")) +
    labs(fill = tr("Survey"),
         colour = tr("Survey"))

  g
}
