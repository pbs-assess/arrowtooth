#' Plot the mean condition factor by year with confidence intervals
#'
#' @param fit_lst A list of a fit from [sdmTMB] and a data frame
#' of the data used in that fit as returned by [condition_fit()]
#' @param grid_fn The Synoptic grid data frame
#'
#' @returns A [ggplot2::ggplot()] object
#' @export2
condition_factor <- \(
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

  p <- predict(fit, newdata = nd, nsim = 300)
  ind <- get_index_sims(p,
                        agg_function = \(x) mean(x),
                        area_function = \(x, area) x * area)

  g <- ggplot(ind,
         aes(year,
             exp(est),
             ymin = exp(lwr),
             ymax = exp(upr))) +
    geom_ribbon(alpha = 0.5) +
    geom_line() +
    theme_pbs() +
    ylab(ifelse(fr(),
                "Facteur de condition",
                "Condition factor")) +
    xlab(tr("Year"))

  g
}
