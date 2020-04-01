library(tidyverse)
library(furrr)
library(future)
library(here)
library(csasdown)
library(gfutilities)
library(gfiscamutils)
library(rosettafish)

models_dir <- "models"

base_model_dir <- "01-base"
base_model_dir_full <- here::here(models_dir, base_model_dir)

if(!exists("regen_rds_files")){
  regen_rds_files <- FALSE
}

# A list of lists of sensitivity groups. Any given sensitivity can appear in more than one group.
# Do not include base model in this list or it will appear twice on plots
# The base model name is added to each group with the map..prepend pipe at the end
sens_models_dirs <- list(
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

unique_models_dirs <- sens_models_dirs %>%
  flatten() %>%
  unique() %>%
  map_chr(.f = ~{.x})

unique_models_dirs_full <- here::here(models_dir, unique_models_dirs)

nul <- map(unique_models_dirs_full, ~{create_rds_file(.x, overwrite = regen_rds_files)})

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

# build_rds_files(major_model_dirs,
#                 mcmc.subdir = "mcmc",
#                 load.proj = TRUE,
#                 lower = confidence_vals[1],
#                 upper = confidence_vals[2],
#                 burnin = 1000,
#                 thin = 1)
