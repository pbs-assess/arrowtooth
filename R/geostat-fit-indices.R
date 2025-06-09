#' Fit the Geostatistical models for Arrowtooth
#'
#' @param dr Directory name where 'data' directory resides
#' @param fn The name of the file containing output from
#' [gfdata::cache_pbs_data()]
#'
#' @return A data frame containing index fits
#' @export
geostat_fit_indices <- \(
    dr = "/srv/arrowtooth/arrowtooth-nongit",
    fn = file.path("/srv/arrowtooth/arrowtooth-nongit/data",
    "arrowtooth-flounder-aug11-2022.rds"),
    lst_species = "arrowtooth flounder"){

  dat <- geostat_prep_data(fn = fn)

  dr_data <- file.path(dr, "data")

  # Delta-gamma-depth
  dgd_fn <- file.path(dr_data, "geo-delta-gamma-depth.rds")
  # Tweedie depth
  td_fn <- file.path(dr_data, "geo-tweedie-depth.rds")
  # Tweedie nodepth
  tnd_fn <- file.path(dr_data, "geo-tweedie-nodepth.rds")
  # Delta-gamma-nodepth
  dgnd_fn <- file.path(dr_data, "geo-delta-gamma-nodepth.rds")

  if(file.exists(dgd_fn)){
    out_dgd <- readRDS(dgd_fn)
  }else{
    out_dgd <- map(lst_species,
                   ~geostat_fit_index(dat,
                                      species = .x,
                                      family = delta_gamma())) |>
      setNames(lst_species)
    saveRDS(out_dgd, file = dgd_fn)
  }

  if(file.exists(td_fn)){
    out_td <- readRDS(td_fn)
  }else{
    out_td <- map(lst_species,
                  ~geostat_fit_index(dat,
                                     species = .x)) |>
      setNames(lst_species)
    saveRDS(out_td, file = td_fn)
  }

  if(file.exists(tnd_fn)){
    out_tnd <- readRDS(tnd_fn)
  }else{
    out_tnd <- map(lst_species,
                   ~geostat_fit_index(dat,
                                      species = .x,
                                      formula = catch_weight ~ 1)) |>
      setNames(lst_species)
    saveRDS(out_tnd, file = tnd_fn)
  }

  if(file.exists(dgnd_fn)){
    out_dgnd <- readRDS(dgnd_fn)
  }else{
    out_dgnd <- map(lst_species,
                    ~geostat_fit_index(dat,
                                       species = .x,
                                       formula = catch_weight ~ 1,
                                       family = delta_gamma())) |>
      setNames(lst_species)
    saveRDS(out_dgnd, file = dgnd_fn)
  }

  index_dgd <- out_dgd |>
    map_dfr("index", .id = "species")|>
    mutate(type = "Delta-Gamma s(depth)")
  index_dgnd <- out_dgnd |>
    map_dfr("index", .id = "species") |>
    mutate(type = "Delta-Gamma")
  index_td <- out_td |>
    map_dfr("index", .id = "species") |>
    mutate(type = "Tweedie s(depth)")
  index_tnd <- out_tnd |>
    map_dfr("index", .id = "species") |>
    mutate(type = "Tweedie")

  index_dgd |>
    bind_rows(index_dgnd) |>
    bind_rows(index_td) |>
    bind_rows(index_tnd) |>
    group_by(species, type) |>
    mutate(includes_depth = grepl("depth", type)) |>
    mutate(max_log = max(log_est)) |>
    filter(max_log < 20) |>
    mutate(species = str_to_title(species))
}