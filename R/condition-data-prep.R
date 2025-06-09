#' Prepare data for body condition modelling
#'
#' @param d A list, as output from [gfdata::get_survey_sets()]
#' @param survey_abbrev A survey abbreviation as found in `d$survey_abbrev`
#'
#' @return A data frame of survey sets
#' @export
condition_data_prep <- function(d,
                                survey_abbrev = c("SYN QCS",
                                                  "SYN HS",
                                                  "SYN WCVI",
                                                  "SYN WCHG")){

  d <- d |>
    filter(survey_abbrev %in% !!survey_abbrev) |>
    mutate(density = density_kgpm2 * 1e6) |>
    mutate(log_depth = log(depth_m)) |>
    mutate(area_swept1 = doorspread_m * speed_mpm * duration_min) |>
    mutate(area_swept2 = doorspread_m * tow_length_m) |>
    mutate(area_swept = ifelse(!is.na(area_swept2),
                               area_swept2,
                               area_swept1)) |>
    filter(!is.na(area_swept)) |>
    sdmTMB::add_utm_columns(c("longitude", "latitude"),
                            utm_crs = 32609)

  d
}
