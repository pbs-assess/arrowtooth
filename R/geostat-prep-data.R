#' Prepare data for Geostatistical spatial model
#'
#' @param fn Filename of the RDS file which includes output list from
#' [gfdata::cache_pbs_data()]
#' @param region A vector of region abbreviations to include in the data
#' @param utm_crs A Universal Transverse Mercator Coordinate Reference System
#' number
#'
#' @return A data frame containing survey set data with area swept
#' calculations and UTM coordinates
#'
#' @export
geostat_prep_data <- \(
  fn = "/srv/arrowtooth/arrowtooth-nongit/data/arrowtooth-flounder-aug11-2022.rds",
  region = c("QCS", "HS", "WCVI", "WCHG"),
  utm_crs = 32609){

  survey_abbrevs_vec <- paste0("SYN ", region)

  if(!file.exists(fn)){
    stop("File does not exist:\n", fn)
  }
  dat <- readRDS(fn)
  ss <- dat$survey_sets |>
    filter(survey_abbrev %in% survey_abbrevs_vec)

  ss <- ss |>
    mutate(density = density_kgpm2 * 1e6) |>
    mutate(log_depth = log(depth_m)) |>
    mutate(area_swept1 = doorspread_m * speed_mpm * duration_min) |>
    mutate(area_swept2 = tow_length_m * doorspread_m) |>
    mutate(area_swept = ifelse(!is.na(area_swept2),
                               area_swept2,
                               area_swept1)) |>
    filter(!is.na(area_swept)) |>
    add_utm_columns(c("longitude", "latitude"),
                    utm_crs = utm_crs)

  ss
}