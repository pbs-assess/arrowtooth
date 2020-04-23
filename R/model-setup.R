# These assignments to NULL are required to avoid check() warnings.
# The function model_setup() must be run to assign these variables to values.
nongit_dir <- NULL
models_dir <- NULL
base_model_dir <- NULL
base_model_dir_full <- NULL
regen_rds_files <- NULL
sens_models_dirs <- NULL
sens_models <- NULL
unique_models_dirs <- NULL
unique_models_dirs_full <- NULL
unique_models <- NULL
base_model <- NULL

model_setup <- function(){
  nongit_dir <<- file.path("..", "arrowtooth-nongit")
  models_dir <<- file.path(nongit_dir, "models")

  base_model_dir <<- "01-base"
  base_model_dir_full <<- file.path(models_dir, base_model_dir)

  if(!exists("regen_rds_files")){
    regen_rds_files <<- FALSE
  }

  # A list of lists of sensitivity groups. Any given sensitivity can appear in more than one group.
  # Do not include base model in this list or it will appear twice on plots
  # The base model name is added to each group with the map..prepend pipe at the end
  sens_models_dirs <<- list(
    c("02-sigma-0.1",
      "03-estimated-total-variance"),
    c("04-tau-1.0",
      "05-tau-0.6"),
    "06-low-steepness",
    c("07-m-0.2-sd-0.05",
      "08-m-0.2-sd-0.25"),
    "09-m-0.3-sd-0.2",
    "10-qm0-qsd1",
    "11-qm05-qsd15",
    c("12-fix-trawl-sel-amat",
      "13-fix-trawl-sel-6years")
  ) %>%
    map(~{prepend(.x, base_model_dir, before = 1)})

  unique_models_dirs <<- sens_models_dirs %>%
    flatten() %>%
    unique() %>%
    map_chr(.f = ~{.x})

  unique_models_dirs_full <<- file.path(models_dir, unique_models_dirs)

  nul <- map(unique_models_dirs_full, ~{create_rds_file(.x, overwrite = regen_rds_files)})

  # This ensures that each unique model is loaded only once, even if it is in multiple
  # sensitivity groups
  unique_models <<- map(unique_models_dirs_full, ~{load_rds_file(.x)}) %>%
    set_names(unique_models_dirs)

  base_model <<- unique_models[[match(base_model_dir, unique_models_dirs)]]
  # sens_models is a list of lists of sensitivities of the same structure as sens_models_dirs.
  # the base_model is first in each sensitivity group list
  sens_models <<- map(sens_models_dirs, ~{
    map(.x, ~{
      unique_models[[match(.x, unique_models_dirs)]]
    })
  })
}