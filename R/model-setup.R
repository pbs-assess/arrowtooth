#' Set main directories for the project
#'
#' @param nongit_dir The directory containing non-version-controlled things
#' such as data, model runs from iscam, and reference materials
#' @param models_dir The name of the directory in which the iscam model
#' directories are located
#' @param base_model_dir Name of the base model directory
#'
#' @return A list of 4 items, the first three are the same as the input arguments
#' and the 4th is the full path of the base model directory
#' @export
set_dirs <- function(nongit_dir = file.path(dirname(here::here()), "arrowtooth-nongit"),
                     models_dir = file.path(nongit_dir, "models"),
                     base_model_dir = "01-base",
                     bridge_models_dir = "001-bridge-models",
                     bridge_base_model_dir = "01-base"){

  list(nongit_dir = nongit_dir,
       models_dir = models_dir,
       base_model_dir = base_model_dir,
       base_model_dir_full = file.path(models_dir, base_model_dir),
       bridge_models_dir = bridge_models_dir,
       bridge_models_dir_full = file.path(models_dir, bridge_models_dir),
       bridge_base_model_dir_full = file.path(models_dir, bridge_models_dir, bridge_base_model_dir))
}

#' Load models and set up the directory names
#'
#' @param main_dirs A list of four, with the same names as the output
#' from [set_dirs()]
#' @param overwrite_rds_files Logical. TRUE to overwrite the model RDS files
#'
#' @return A list of two items, the base_model and the list of sensitivity models,
#' which itself it composed of lists of groups of sensitivities which are to be compared with
#' each other in the document. This simplifies plotting and table functions.
#' @importFrom gfiscamutils create_rds_file
#' @export
model_setup <- function(main_dirs = set_dirs(),
                        overwrite_rds_files = FALSE,
                        bridge_models_dirs = list(
                          c("01-base",
                            "02-bridge-update-data",
                            "03-bridge-add-discard-cpue",
                            "04-bridge-add-splitsex-agecomps"))){

  nongit_dir <- main_dirs$nongit_dir
  models_dir <- main_dirs$models_dir
  base_model_dir <- main_dirs$base_model_dir
  base_model_dir_full <- main_dirs$base_model_dir_full
  bridge_models_dir <- main_dirs$bridge_models_dir
  bridge_models_dir_full <- main_dirs$bridge_models_dir_full
  bridge_base_model_dir_full <- main_dirs$bridge_base_model_dir_full

  # A list bridge models. The last assessment's base model should appear first in this list.
  bridge_models_dirs <- bridge_models_dirs %>%
    map(~{append(.x, base_model_dir, after = 0)})

  sens_models_dirs <- NULL

  # A list of lists of sensitivity groups. Any given sensitivity can appear in more than one group.
  # Do not include base model in this list or it will appear twice on plots
  # The base model name is prepended to each group with the map..append pipe at the end
  # sens_models_dirs <- list(
  #   c("02-sigma-0.1",
  #     "03-estimated-total-variance"),
  #   c("04-tau-1.0",
  #     "05-tau-0.6"),
  #   "06-low-steepness",
  #   c("07-m-0.2-sd-0.05",
  #     "08-m-0.2-sd-0.25"),
  #   "09-m-0.3-sd-0.2",
  #   "10-qm0-qsd1",
  #   "11-qm05-qsd15",
  #   c("12-fix-trawl-sel-amat",
  #     "13-fix-trawl-sel-6years")
  # ) %>%
  #   map(~{append(.x, base_model_dir, after = 0)})

  if(is.null(sens_models_dirs)){
    sens_models <- NULL
  }else{
    unique_models_dirs <- sens_models_dirs %>%
      flatten() %>%
      unique() %>%
      map_chr(~{.x})

    unique_models_dirs_full <- file.path(models_dir, unique_models_dirs)

    nul <- map(unique_models_dirs_full, ~{create_rds_file(.x, overwrite = overwrite_rds_files)})

    # This ensures that each unique model is loaded only once, even if it is in multiple
    # sensitivity groups
    unique_models <- map(unique_models_dirs_full, ~{load_rds_file(.x)}) %>%
      set_names(unique_models_dirs)

    base_model <- unique_models[[match(base_model_dir, unique_models_dirs)]]
    # sens_models is a list of lists of sensitivities of the same structure as sens_models_dirs.
    # the base_model is first in each sensitivity group list
    sens_models <- map(sens_models_dirs, ~{
      map(.x, ~{
        unique_models[[match(.x, unique_models_dirs)]]
      })
    })
  }

  list(base_model = base_model,
       sens_models = sens_models)
}