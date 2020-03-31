library(tidyverse)
library(furrr)
library(future)
library(here)
library(csasdown)
library(gfutilities)
library(gfiscamutils)
library(rosettafish)

base_model_dir <- "01-base"

# A list of lists of sensitivity groups. Any given sensitivity can appear in more than one group.
# Do not include base model in this list or it will appear twice on plots
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

unique_models_dirs_full <- here::here("models", unique_models_dirs)

#create_rds_file(here::here("models", base_model_dir))

# build_rds_files(major_model_dirs,
#                 mcmc.subdir = "mcmc",
#                 load.proj = TRUE,
#                 lower = confidence_vals[1],
#                 upper = confidence_vals[2],
#                 burnin = 1000,
#                 thin = 1)
