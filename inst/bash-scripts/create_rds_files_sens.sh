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

 models=(01-sigma-0.135 \
         02-tau-1.0 \
         03-tau-0.6 \
         04-low-steepness \
         05-m-female-tight-prior \
         06-m-female-loose-prior \
         07-m-male-tight-prior \
         08-m-male-loose-prior \
         09-qk-mean-1.0 \
         10-qk-loose-prior \
         11-selex-equal-maturity \
         12-geostat-surveys)

project_path=`Rscript -e "cat(dirname(here::here()))"`
models_path=$project_path/arrowtooth-nongit/models
model_group=02-sens-models

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Creating RDS file for $model in a subshell"; \
  Rscript -e "library(tidyverse);library(gfiscamutils);create_rds_file('$models_path/$model_group/$model')"
  echo "$model RDS file created") &

done
