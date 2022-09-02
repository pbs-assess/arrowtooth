#!/bin/bash
# Rename a fleet in the bridge models
#
# Run this to fix 'bad interpreter' issue:
# sed -i -e 's/\r$//' rename_fleet_bridge.sh

 models=(01-base-2015 \
         02-bridge-update-data-to-2014 \
         03-bridge-update-data-to-2021 \
         04-bridge-add-wchg \
         05-bridge-switch-to-dm-likelihood \
         06-bridge-switch-to-2-fleet-model \
         07-bridge-add-discard-cpue \
         08-bridge-switch-to-split-sex \
         09-bridge-switch-fishing-year-to-feb-21-feb-20 \
         10-remove-wchg)

project_path=`Rscript -e "cat(dirname(here::here()))"`
models_path=$project_path/arrowtooth-nongit/models-input-files-test
model_group=01-bridge-models

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Renaming fleet for $model in a subshell"; \
  Rscript -e "gfiscamutils::rename_gear_iscam('$models_path/$model_group/$model/arf.dat', 'Wet boats', 'Shoreside')"; \
  Rscript -e "gfiscamutils::rename_gear_iscam('$models_path/$model_group/$model/arf.dat', 'WB', 'SS')"; \
  echo "Completed renaming 'Wet boat' fleet in model $model") &

done
