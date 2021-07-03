#' Set main directories for the project. Check existence of all directories and report
#'
#' @param nongit_dir The full path containing non-version-controlled things
#' such as data, model runs from iSCAM, and reference materials
#' @param models_dir The full path in which the iSCAM model directories are located
#' @param base_model_dir Name of the base model directory which is a subdirectory of `models`
#' @param bridge_models_dir Name of the subdirectory of `models_dir` that
#' contains the bridge model directories
#' @param bridge_models_dirs A vector of subdirectory names in `models_dir/bridge_models_dir`
#'  that each contain an individual iSCAM bridge model
#' @param sens_models_dir Name of the subdirectory of `models_dir` that
#' contains the sensitivity model directories
#' @param sens_models_dirs A vector of subdirectory names in `models_dir/sens_models_dir`
#'  that each contain an individual iSCAM sensitivity model
#'
#' @return A list of five items, the first two are the same as the input arguments.
#' The third is the full path of the base model directory, the fourth and fifth contain vectors
#' of the bridge models' directory names and the sensitivity models' directory names respectively
#' @importFrom purrr map_lgl
#' @export
set_dirs <- function(nongit_dir = file.path(dirname(here()), "arrowtooth-nongit"),
                     models_dir = file.path(nongit_dir, "models"),
                     base_model_dir = "base",
                     bridge_models_dir = "001-bridge-models",
                     bridge_models_dirs = NULL,
                     sens_models_dir = "002-sens-models",
                     sens_models_dirs = NULL){

  stopifnot(!is.null(nongit_dir))
  if(!dir.exists(nongit_dir)){
    stop("Non-Git directory does not exist:\n",
         nongit_dir, call. = FALSE)
  }
  stopifnot(!is.null(models_dir))
  if(!dir.exists(models_dir)){
    stop("Models directory does not exist:\n",
         models_dir, call. = FALSE)
  }
  stopifnot(!is.null(base_model_dir))
  base_model_dir_full <- file.path(models_dir, base_model_dir)
  if(!dir.exists(base_model_dir_full)){
    stop("Base model directory does not exist:\n",
         base_model_dir_full, call. = FALSE)
  }

  bridge_models_dir_full <- file.path(models_dir, bridge_models_dir)
  if(!dir.exists(bridge_models_dir_full)){
    stop("Bridge models directory does not exist:\n",
         bridge_models_dir_full, call. = FALSE)
  }
  bridge_models_dirs_full <- NULL
  if(!is.null(bridge_models_dirs)){
    bridge_models_dirs_full <- file.path(bridge_models_dir_full, bridge_models_dirs)
    dir_existence <- map_lgl(bridge_models_dirs_full, ~{dir.exists(.x)})
    if(!all(dir_existence)){
      stop("Some bridge model directories do not exist:\n",
           paste0(bridge_models_dirs_full[!dir_existence], collapse = "\n"),
           call. = FALSE)
    }
  }

  stopifnot(!is.null(sens_models_dir))
  sens_models_dir_full = file.path(models_dir, sens_models_dir)
  if(!dir.exists(sens_models_dir_full)){
    stop("Sensitivity models directory does not exist:\n",
         sens_models_dir_full, call. = FALSE)
  }
  sens_models_dirs_full <- NULL
  if(!is.null(sens_models_dirs)){
    sens_models_dirs_full <- file.path(sens_models_dir_full, sens_models_dirs)
    dir_existence <- map_lgl(sens_models_dirs_full, ~{dir.exists(.x)})
    if(!all(dir_existence)){
      stop("Some Sensitivity model directories do not exist:\n",
           paste0(sens_models_dirs_full[!dir_existence], collapse = "\n"),
           call. = FALSE)
    }
  }

  list(nongit_dir = nongit_dir,
       models_dir = models_dir,
       base_model_dir = base_model_dir_full,
       bridge_models_dirs = bridge_models_dirs_full,
       sens_models_dirs = sens_models_dirs_full)
}

#' Load models and set up the directory names
#'
#' @param main_dirs A list of four, with the same names as the output
#' from [set_dirs()]
#' @param overwrite_rds_files Logical. TRUE to overwrite the model RDS files
#' @param bridge_models_dirs A vector of names of directories containing bridge models.
#' Reference directories only, not full paths
#'
#' @return A list of two items, the base_model and the list of sensitivity models,
#' which itself it composed of lists of groups of sensitivities which are to be compared with
#' each other in the document. This simplifies plotting and table functions.
#' @importFrom gfiscamutils create_rds_file
#' @importFrom purrr map_chr
#' @export
model_setup <- function(main_dirs = set_dirs(),
                        overwrite_rds_files = FALSE,
                        bridge_models_dirs = NULL,
                        sens_models_dirs = NULL){

  if(is.null(bridge_models_dirs) && is.null(sens_models_dirs)){
    stop("Both bridge_models_dirs and sens_models_dirs are NULL", call. = FALSE)
  }
  if(is.null(bridge_models_dirs)){
    bridge_models_dirs_full <- NULL
  }else{
    bridge_models_dirs_full <- file.path(main_dirs$bridge_models_dir_full, bridge_models_dirs)
  }
  if(is.null(sens_models_dirs)){
    sens_models_dirs_full <- NULL
  }else{
    sens_models_dirs_full <- file.path(main_dirs$sens_models_dir_full, sens_models_dirs)
  }

  browser()
  bridge_models_dirs <- NULL
  sens_models_dirs <- NULL

  j <- map(list(bridge_models_dirs_full, sens_models_dirs_full), ~{
    models <- NULL
    if(!is.null(.x)){
      unique_models_dirs <- .x %>%
        flatten() %>%
        unique() %>%
        map_chr(~{.x})

      unique_models_dirs_full <- file.path(models_dir, unique_models_dirs)

      nul <- map(unique_models_dirs_full, ~{create_rds_file(.x, overwrite = overwrite_rds_files)})

      # This ensures that each unique model is loaded only once, even if it is in multiple
      # sensitivity groups
      unique_models <- map(unique_models_dirs_full, ~{load_rds_file(.x)}) %>%
        set_names(unique_models_dirs)

      #base_model <- unique_models[[match(base_model_dir, unique_models_dirs)]]
      # sens_models is a list of lists of sensitivities of the same structure as sens_models_dirs.
      # the base_model is first in each sensitivity group list
      models <- map(.x, ~{
        map(.x, ~{
          unique_models[[match(.x, unique_models_dirs)]]
        })
      })
    }
  })

  list(base_model = base_model,
       bridge_models = bridge_models,
       sens_models = sens_models)
}