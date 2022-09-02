#!/bin/bash
# Rename a fleet in the sensitivity models
#
# Run this to fix 'bad interpreter' issue:
# sed -i -e 's/\r$//' rename_fleet_sens.sh

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

project_path=`Rscript -e "cat(dirname(here::here()))"`
models_path=$project_path/arrowtooth-nongit/models-input-files-test
model_group=02-sens-models

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Renaming fleet for $model in a subshell"; \
  Rscript -e "gfiscamutils::rename_gear_iscam('$models_path/$model_group/$model/arf.dat', 'Wet boats', 'Shoreside')"; \
  Rscript -e "gfiscamutils::rename_gear_iscam('$models_path/$model_group/$model/arf.dat', 'WB', 'SS')"; \
  echo "Completed renaming 'Wet boat' fleet in model $model") &

done
