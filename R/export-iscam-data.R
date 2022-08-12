#' Export all iSCAM data for pasting into the iSCAM data files
#'
#' @param dat Output from [gfdata::cache_pbs_data()]. Make sure to set the
#' argument `unsorted_only = FALSE` for arrowtooth
#'
#' @return Nothing, creates several files for cutting/pasting into iSCAM
#' data files
#' @export
export_iscam_data <- function(dat){

  export_catch(dat$catch)
  export_mat_lw_age(dat$survey_samples)
  export_survey_indices(dat$survey_index)

  commercial_samples <- dat$commercial_samples |>
    filter(major_stat_area_code %in% c("03","04", "05", "06", "07", "08", "09"))
  comm_ft <- extract_fleet_samples(commercial_samples)
  comm_ss <- extract_fleet_samples(commercial_samples, include = FALSE)
  catch_ft <- extract_fleet_catch(dat$catch)
  catch_ss <- extract_fleet_catch(dat$catch, include = FALSE)
  export_age_comps(catch_ft, comm_ft, gear_num = 1, fn_append = "freezer-trawlers")
  export_age_comps(catch_ss, comm_ss, gear_num = 2, fn_append = "shoreside")

  survey_sets <- dat$survey_sets |>
    filter(major_stat_area_code %in% c("03","04", "05", "06", "07", "08", "09"))
  survey_samples <- dat$survey_samples |>
    filter(major_stat_area_code %in% c("03","04", "05", "06", "07", "08", "09"))
  export_age_comps(survey_sets,
                   survey_samples,
                   type = "survey",
                   gear_num = 3,
                   surv_series_name = "SYN QCS",
                   fn_append = "qcs")

  export_age_comps(survey_sets,
                   survey_samples,
                   type = "survey",
                   gear_num = 5,
                   surv_series_name = "SYN HS",
                   fn_append = "hs")

  export_age_comps(survey_sets,
                   survey_samples,
                   type = "survey",
                   gear_num = 6,
                   surv_series_name = "SYN WCVI",
                   fn_append = "wcvi")

  export_age_comps(survey_sets,
                   survey_samples,
                   type = "survey",
                   gear_num = 7,
                   surv_series_name = "SYN WCHG",
                   fn_append = "wchg")

}
