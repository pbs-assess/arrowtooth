#!/bin/bash
# Create sensitivity model RDS files in parallel
#
# Note that if you are working in Windows and running this script in WSL2, the
# Linux version of R is run, so if there are any changes to gfiscamutils,
# you must have pushed them to Github and then entered R in Linux and run:
# remotes::install_github("pbs-assess/gfiscamutils")
#
# Run this to fix 'bad interpreter' issue:
# sed -i -e 's/\r$//' create_rds_files_sens.sh

 models=(01-sigma-0.1 \
         02-estimate-total-variance \
         03-tau-1.0 \
         04-tau-0.6 \
         05-low-steepness \
         06-m-female-tight-prior \
         07-m-female-loose-prior \
         08-m-male-tight-prior \
         09-m-male-loose-prior \
         10-qk-mean-1.0 \
         11-qk-loose-prior \
         12-selex-equal-maturity \
         13-geostat-surveys)

project_path=`Rscript -e "cat(here::here())"`
models_path=$project_path/arrowtooth-nongit/models-mcmc-runs
model_group=02-sens-models

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Creating RDS file for $model in a subshell"; \
  Rscript -e "library(tidyverse);library(gfiscamutils);create_rds_file('$models_path/$model_group/$model')"
  echo "$model RDS file created") &

done
