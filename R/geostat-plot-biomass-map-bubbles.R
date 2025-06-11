#' Plot spatial maps showing biomass density, one for each year
#'
#' @param d Output from [geostat_prep_data()]
#' @param deg_offset Degrees offset vector of 2 values for coastline
#' @param meters_offset Meters UTM offset for coordinates used in limits of
#' plot
#'
#' @return A [ggplot2::ggplot()] object
#' @export
geostat_plot_biomass_map_bubbles <- \(d,
                                      deg_offset = c(-0.5, 0.5),
                                      meters_offset = c(-10, 10),
                                      utm_zone = 9){

  if(fr()){
    density_lab <- bquote(atop("Densité de la biomasse", ~ (kg / km^2)))
  }else{
    density_lab <- bquote(atop("Biomass density", ~ (kg / km^2)))
  }

  coast <- gfplot:::load_coastline(range(d$longitude) + deg_offset,
                                   range(d$latitude) + deg_offset,
                                   utm_zone = utm_zone)
  coords <- coord_equal(expand = FALSE,
                        xlim = range(d$X) + meters_offset,
                        ylim = range(d$Y) + meters_offset)


  # d <- d |>
  #   rename(density = density_kgpm2)
  d0 <- d |>
    filter(density == 0)
  d <- d |>
    filter(density > 0)


  g <- d |>
    ggplot(aes(X,
               Y,
               size = density,
               fill = density,
               colour = density)) +
    scale_size_area(max_size = 8) +
    geom_point(data = d0,
               pch = 4,
               size = 0.7,
               col = "grey60") +
    coords +
    geom_point(alpha = 0.5, pch = 21) +
    facet_wrap(~year) +
    geom_polygon(
      data = coast,
      aes_string(x = "X",
                 y = "Y",
                 group = "PID"),
      fill = "grey87",
      col = "grey70",
      lwd = 0.2,
      inherit.aes = FALSE) +
    scale_fill_viridis_c(trans = "sqrt") +
    scale_colour_viridis_c(trans = "sqrt") +
    labs(fill = density_lab,
         colour = density_lab,
         size = density_lab,
         x = tr("Easting"),
         y = tr("Northing")) +
    guides(size = guide_legend(order = 1),
           fill = guide_colorbar(order = 0),
           colour = "none") +
    theme_pbs()

  g
}
