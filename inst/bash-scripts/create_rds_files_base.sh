#!/bin/bash
# Create base models RDS files in parallel
#
# Note that if you are working in Windows and running this script in WSL2, the
# Linux version of R is run, so if there are any changes to gfiscamutils,
# you must have pushed them to Github and then entered R in Linux and run:
# remotes::install_github("pbs-assess/gfiscamutils")
#
# Run this to fix 'bad interpreter' issue:
# sed -i -e 's/\r$//' create_rds_files_base.sh

project_path=`Rscript -e "cat(dirname(here::here()))"`
model_path=$project_path/arrowtooth-nongit/models/base

echo
echo "Creating RDS file for base model in a subshell"
echo
Rscript -e "library(tidyverse);library(gfiscamutils);create_rds_file('$model_path', overwrite = TRUE)"
echo
echo "Base model RDS file created"
echo
