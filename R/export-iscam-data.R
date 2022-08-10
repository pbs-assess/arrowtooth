#' Export all iSCAM data for pasting into the iSCAM data files
#'
#' @param dat Output from [gfdata::cache_pbs_data()]
#'
#' @return Nothing, creates several files for cutting/pasting into iSCAM
#' data files
#' @export
export_iscam_data <- function(dat){

  export_catch(dat$catch)
  export_mat_lw_age(dat$survey_samples)
  export_survey_indices(dat$survey_index)

}