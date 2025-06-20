geostat_plot_survey_index_models <- \(
  fn_models = "/srv/arrowtooth/arrowtooth-nongit/geostat-figs/geo-delta-gamma-depth.rds",
  fn_discards = file.path("/srv/arrowtooth/arrowtooth-nongit/data",
                          paste0("cpue-predictions-arrowtooth-flounder-modern-",
                                 "3CD5ABCDE-discard-july-26-feb-fishing-year.csv"))){

  discard_cpue <- read_csv(fn_discards) |>
    filter(formula_version == "Full standardization")


  dr <- dirname(fn_models)
  if(!dir.exists(dr)){
    bail("Directory `", dr, "` does not exist")
  }

  if (!file.exists(fn_models)) {

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

  ind <- index |>
    bind_rows(index_nodepth) |>
    bind_rows(index_dg) |>
    bind_rows(index_dg_nodepth) |>
    mutate(cv = sqrt(exp(se^2) - 1))

  ind_cent <- ind |>
    group_by(type) |>
    mutate(geo_mean = exp(mean(log_est))) |>
    mutate(est = est / geo_mean,
           lwr = lwr / geo_mean,
           upr = upr / geo_mean,
           type = paste0("Survey ", type),
           type2 = "a-survey")

  cpue_cent <- discard_cpue |>
    mutate(geo_mean = exp(mean(est_link[year %in% 2003:2021]))) |>
    mutate(est = est / geo_mean,
           lwr = lwr / geo_mean,
           upr = upr / geo_mean,
           type = "Discard CPUE",
           type2 = "b-cpue")

  comb <- cpue_cent |>
    bind_rows(ind_cent)

  levels(comb$type) <- c("Discard CPUE",
                         "Survey Tweedie s(depth)",
                         "Survey Tweedie",
                         "Survey Delta-Gamma s(depth)",
                         "Survey Delta-Gamma")

  cols <- c("grey30", RColorBrewer::brewer.pal(4, "Set2"))

  g <- comb |>
    ggplot(aes(year,
               est,
               color = type,
               fill = type,
               ymin = lwr,
               ymax = upr,
               lty = type2)) +
    theme_pbs() +
    geom_ribbon(alpha = 0.3, colour = NA) +
    geom_line() +
    scale_fill_manual(values = cols) +
    scale_color_manual(values = cols) +
    labs(fill = "Type", colour = "Type") +
    ylab(ifelse(fr(),
                "Indice pondéré",
                "Scaled index")) +
    xlab(tr("Year")) +
    guides(lty = "none")

  g
}