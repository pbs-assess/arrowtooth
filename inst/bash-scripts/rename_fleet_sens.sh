#!/bin/bash
# Rename a fleet in the sensitivity models
#
# Run this to fix 'bad interpreter' issue:
# sed -i -e 's/\r$//' rename_fleet_sens.sh

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
         12-geostat-surveys \
         13-qcs-tv-selex \
         14-fix-dm-phi)

project_path=`Rscript -e "cat(dirname(here::here()))"`
models_path=$project_path/arrowtooth-nongit/models
model_group=02-sens-models

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Renaming fleet for $model in a subshell"; \
  Rscript -e "gfiscamutils::rename_gear_iscam('$models_path/$model_group/$model/arf.dat', 'Wet boats', 'Shoreside')"; \
  Rscript -e "gfiscamutils::rename_gear_iscam('$models_path/$model_group/$model/arf.dat', 'WB', 'SS')"; \
  echo "Completed renaming 'Wet boat' fleet in model $model") &

done
