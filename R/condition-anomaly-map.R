#' Plot the condition anomaly map, i.e. plumpness factor
#'
#' @param fit_lst A list of a fit from [sdmTMB] and a data frame
#' of the data used in that fit as returned by [condition_fit()]
#' @param grid_fn The Synoptic grid data frame
#'
#' @returns A [ggplot2::ggplot()] object
#' @export
condition_anomaly_map <- \(fit_lst,
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

  pred_map <- predict(fit, newdata = nd)

  coast <- gfplot:::load_coastline(
    range(ds$longitude) + c(-0.5, 0.5),
    range(ds$latitude) + c(-0.5, 0.5),
    utm_zone = 9)

  g <- ggplot(pred_map, aes(X, Y, fill = est)) +
    geom_tile(width = 2, height = 2) +
    facet_wrap(~year) +
    scale_fill_gradient2() +
    geom_polygon(data = coast,
                 aes_string(x = "X", y = "Y", group = "PID"),
                 fill = NA,
                 col = "grey70",
                 lwd = 0.2,
                 inherit.aes = FALSE) +
    gfplot::theme_pbs() +
    coord_fixed(expand = FALSE,
                xlim = range(pred_map$X) + c(-10, 10),
                ylim = range(pred_map$Y) + c(-10, 10)) +
    labs(fill = ifelse(fr(),
                       "Anomalie de condition\n(espace logarithmique)",
                       "Condition anomaly\n(log space)"))

  g
}