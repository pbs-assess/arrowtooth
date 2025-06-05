#' Extract CPUE for Arrowtooth Flounder
#'
#' @details
#' Used for the plots in the CPUE appendix in the 2022 assessment
#'
#' @param fn_cpue The file name of the cpue index data used
#' @param ret_params If `TRUE`, return `params` list instead
#'
#' @return A data frame, or the `params` list, if `ret_params` is `TRUE`
#' @export
cpue_extraction <- \(fn_cpue = file.path(drs$nongit_dir,
                                         "cpue-figs",
                                         "arrowtooth-cpue-to-2024-10-21.rds"),
                     params,
                     ret_params = FALSE){
    # fn_cpue_predict = file.path(
    #   drs$nongit_dir,
    #   "data",
    #   "cpue-predictions-arrowtooth-flounder-modern-3CD5ABCDE-2024.csv"

  params = list(
    species_proper = "Arrowtooth Flounder",
    area = c("^5A|^5B|^5C|^5D|^5E|^3C|^3D"),
    area_name = c("3CD5ABCDE"),
    skip_single_variable_models = FALSE,
    use_alt_year = FALSE,
    alt_year_start_date = "02-21",
    final_year = 2021,
    final_date = "2021-12-31",
    discard_only = TRUE,
    lat_range = c(48, Inf),
    min_positive_tows = 140,
    min_positive_trips = 10,
    min_yrs_with_trips = 5,
    depth_range = c(-Inf, Inf),
    era = "modern",
    parallel = TRUE
  )

  if(ret_params){
    return(params)
  }

  # The file `fn_cpue` was generated using this command:
  # gfdata::get_cpue_index(gear = "bottom trawl", min_cpue_year = 1996) |>
  # filter(species_common_name = "ARROWTOOTH FLOUNDER)
  comm_cpue <- readRDS(fn_cpue)

  comm_cpue$fishing_event_id_unique <-
    paste0(comm_cpue$vessel_registration_number,
           "-",
           comm_cpue$trip_id,
           "-",
           comm_cpue$fishing_event_id)

  define_fleet <- \(area, area_name){
    out <- gfplot::tidy_cpue_index(comm_cpue,
                                   species_common = tolower(params$species_proper),
                                   gear = "bottom trawl",
                                   alt_year_start_date = params$alt_year_start_date,
                                   use_alt_year = params$use_alt_year,
                                   year_range = c(1996, params$final_year),
                                   lat_range = params$lat_range,
                                   min_positive_tows = params$min_positive_tows,
                                   min_positive_trips = params$min_positive_trips,
                                   min_yrs_with_trips = params$min_yrs_with_trips,
                                   depth_band_width = 25,
                                   area_grep_pattern = area,
                                   depth_bin_quantiles = params$depth_bin_quantiles,
                                   min_bin_prop = 0.001,
                                   lat_band_width = 0.1)
    out$area <- area_name
    out
  }

  if(params$discard_only) {
    comm_cpue <- comm_cpue |>
      dplyr::filter(discarded_kg > 0,
                    landed_kg == 0)
  }
  if(!is.null(params$final_date)) {
    comm_cpue <- comm_cpue |>
      dplyr::filter(best_date <= lubridate::ymd(params$final_date))
  }

  comm_cpue <- comm_cpue |>
    dplyr::filter(best_depth >= params$depth_range[[1]],
                  best_depth <= params$depth_range[[2]])

  dfleet <- define_fleet(params$area, params$area_name)

  dfleet
}