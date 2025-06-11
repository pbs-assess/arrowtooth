#' Plot a time series of the various stitched indices
#'
#' @param d Output from [geostat_prep_data()]
#' @param fn File name for the RDS file containing model outputs
#'
#' @return A [ggplot2::ggplot()] object
#' @export
geostat_plot_stitched_indices <- \(d,
                                   fn = "/srv/arrowtooth/arrowtooth-nongit/geostat-figs/geo-delta-gamma-depth.rds"){

  dr <- dirname(fn)
  if(!dir.exists(dr)){
    bail("Directory `", dr, "` does not exist")
  }

  if (!file.exists(fn)) {

    list_species <- "arrowtooth flounder"

    out <- map(list_species,
               ~fit_index(d,
                          folder = dr,
                          species = .x)) |>
      setNames(list_species)

    out_nodepth <- map(list_species,
                       ~fit_index(d,
                                  folder = dr,
                                  species = .x,
                                  formula = catch_weight ~ 1)) |>
      setNames(list_species)

    out_dg_nodepth <- map(list_species,
                          ~fit_index(d,
                                     folder = dr,
                                     species = .x,
                                     formula = catch_weight ~ 1,
                                     family = delta_gamma())) |>
      setNames(list_species)

    out_dg <- map(list_species,
                  ~fit_index(d,
                             folder = dr,
                             species = .x,
                             family = delta_gamma())) |>
                    setNames(list_species)

    saveRDS(out, file = file.path(dr, "geo-tweedie-depth.rds"))
    saveRDS(out_nodepth, file = file.path(dr, "geo-tweedie-nodepth.rds"))
    saveRDS(out_dg, file = file.path(dr, "geo-delta-gamma-depth.rds"))
    saveRDS(out_dg_nodepth, file = file.path(dr, "geo-delta-gamma-nodepth.rds"))
  }else{
    out <- readRDS(file.path(dr, "geo-tweedie-depth.rds"))
    out_nodepth <- readRDS(file.path(dr, "geo-tweedie-nodepth.rds"))
    out_dg <- readRDS(file.path(dr, "geo-delta-gamma-depth.rds"))
    out_dg_nodepth <- readRDS(file.path(dr, "geo-delta-gamma-nodepth.rds"))
  }

  index_dg_nodepth <- map_dfr(out_dg_nodepth, "index", .id = "species")
  index_nodepth <- map_dfr(out_nodepth, "index", .id = "species")
  index <- map_dfr(out, "index", .id = "species")
  index_dg <- map_dfr(out_dg, "index", .id = "species")

  dpth <- ifelse(fr(),
                 "profondeur",
                 "depth")
  index <- index |>
    mutate(type = paste0("Tweedie s(", dpth,")"))
  index_nodepth <- index_nodepth |>
    mutate(type = "Tweedie")
  index_dg <- index_dg |>
    mutate(type = paste0("Delta-Gamma s(", dpth,")"))
  index_dg_nodepth <- index_dg_nodepth |>
    mutate(type = "Delta-Gamma")

  mult <- 1000

  g <- index |>
    bind_rows(index_nodepth) |>
    bind_rows(index_dg) |>
    bind_rows(index_dg_nodepth) |>
    group_by(species, type) |>
    mutate(includes_depth = grepl(dpth, type)) |>
    mutate(max_log = max(log_est)) |>
    filter(max_log < 20) |>
    mutate(species = stringr::str_to_title(species)) |>
    ggplot(aes(year, est / mult, colour = type, fill = type,
               ymin = lwr / mult, ymax = upr / mult)) +
    theme_pbs()
  g <- g +
    # geom_pointrange(aes(ymin = lwr / mult, ymax = upr / mult))
    geom_ribbon(alpha = 0.25, colour = NA) +
    # geom_line(aes(lty = includes_depth), lwd = 0.8)
    geom_line(lwd = 0.8)
  g <- g + ylab(ifelse(fr(),
                       "Biomasse (tonnes)",
                       "Biomass (tonnes)")) +
    # ggtitle(stringr::str_to_title(species), subtitle = paste(region, collapse = ", ")) +
    coord_cartesian(
      ylim = c(0, 70000),
      expand = FALSE, xlim = range(index$year) + c(-0.25, 0.25)
    ) +
    theme(axis.title.x = element_blank()) +
    labs(colour = ifelse(fr(), "Modèle", "Model"),
         fill = ifelse(fr(), "Modèle", "Model"),
         linetype = paste0(ifelse(fr(), "Comprend", "Includes"),
                           " s(", dpth,")")) +
    scale_fill_brewer(palette = "Set2") +
    scale_colour_brewer(palette = "Set2") +
    scale_linetype_manual(values = c(2, 1))
  g

}