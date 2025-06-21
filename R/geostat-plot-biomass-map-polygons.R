geostat_plot_biomass_map_polygons <- \(d,
                                       pred,
                                       column = "est_total",
                                       deg_offset = c(-0.5, 0.5),
                                       meters_offset = c(-10, 10),
                                       utm_zone = 9,
                                       utm_crs = 32609,
                                       yrs = 2003:2012,
                                       upper_prob = 0.995,
                                       ret_pred = FALSE){

  if(fr()){
    density_lab <- bquote(atop("Densité de la biomasse",
                               ~ (kg / km^2)))
  }else{
    density_lab <- bquote(atop("Biomass density",
                               ~ (kg / km^2)))
  }

  new_col <- sym(column)
  pred <- pred |>
    mutate(!!new_col := plogis(est1) * exp(est2)) |>
    mutate(!!new_col := ifelse(!!new_col >
                                 quantile(!!new_col,
                                          probs = upper_prob),
                              quantile(!!new_col,
                                       probs = upper_prob),
                              !!new_col)) |>
    filter(year %in% yrs)

  if(ret_pred){
    return(pred)
  }

  geostat_plot_map(d,
                   pred,
                   column)
 }
