#' Modify the path element of each of the model RDS files used in the
#' assessment
#'
#' @details
#' Used in the case that the models folders are moved from their original
#' running location. The `path` element was set during the building of
#' the RDS files in that location to their location at that time.
#'
#' @param drs Output from [gfiscamutils::set_dirs()]
#'
#' @return Nothing
#' @export
mod_paths <- function(drs){
  base <- file.path(drs$base_model_dir,
                    paste0(basename(drs$base_model_dir), ".rds"))
  bridge <- file.path(drs$bridge_models_dir,
                      paste0(basename(unique(unlist(drs$bridge_models_dirs))), ".rds"))

  sens <- file.path(drs$sens_models_dir,
                    paste0(basename(unique(unlist(drs$sens_models_dirs))), ".rds"))

  fns <- c(base, bridge, sens)
  modify_model_path(fns, fns)
}
