#!/bin/bash
# Create retrospective model RDS files in parallel
#
# Note that if you are working in Windows and running this script in WSL2, the
# Linux version of R is run, so if there are any changes to gfiscamutils,
# you must have pushed them to Github and then entered R in Linux and run:
# remotes::install_github("pbs-assess/gfiscamutils")
#
# Run this to fix 'bad interpreter' issue:
# sed -i -e 's/\r$//' create_rds_files_retro.sh

models=(01-retrospective-1-years \
        02-retrospective-2-years \
        03-retrospective-3-years \
        04-retrospective-4-years \
        05-retrospective-5-years)

project_path=`Rscript -e "cat(dirname(here::here()))"`
models_path=$project_path/arrowtooth-nongit/models
model_group=03-retrospective-models

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Creating RDS file for $model in a subshell"; \
  Rscript -e "library(tidyverse);library(gfiscamutils);create_rds_file('$models_path/$model_group/$model')"
  echo "$model RDS file created") &

done
