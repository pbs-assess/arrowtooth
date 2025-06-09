#' Plot a coastline map for geostat spatial model output
#'
#' @param d A data.frame as output by [geostat_prep_data()]
#' @param pred A data.frame as output by [stats::predict()]
#' @param column Column name for fill and color
#' @param max_colour The maximum value for color scale
#' @param deg_offset Degrees offset vector of 2 values for coastline
#' @param meters_offset Meters UTM offset for coordinates used in limits of
#' plot
#'
#' @return A [ggplot2::ggplot()] object
#' @export
geostat_plot_map <- \(d,
                      pred,
                      column,
                      max_color = max(pred[[column]]),
                      deg_offset = c(-0.5, 0.5),
                      meters_offset = c(-10, 10),
                      utm_zone = 9){

  fill_col <- sym(column)

  coast <- gfplot:::load_coastline(range(d$longitude) + deg_offset,
                                   range(d$latitude) + deg_offset,
                                   utm_zone = utm_zone)

  coords <- coord_equal(expand = FALSE,
                        xlim = range(d$X) + meters_offset,
                        ylim = range(d$Y) + meters_offset)
  g <- pred |>
    ggplot(aes(X,
               Y,
               fill = !!fill_col,
               colour = !!fill_col)) +
    geom_polygon(data = coast,
                 aes(X,
                     Y,
                     group = PID),
                 fill = "grey87",
                 col = "grey70",
                 lwd = 0.2,
                 inherit.aes = FALSE) +
    geom_tile(width = 2,
              height = 2) +
    scale_colour_viridis_c(trans = "sqrt",
                           limits = c(0, max_color)) +
    scale_fill_viridis_c(trans = "sqrt",
                         limits = c(0, max_color)) +
    coord_fixed() +
    coords +
    labs(x = tr("Easting"),
         y = tr("Northing")) +
    facet_wrap(vars(year), ncol = 3) +
    labs(fill = tr("Biomass density"),
         colour = tr("Biomass density")) +
    theme_pbs()

  g
}
