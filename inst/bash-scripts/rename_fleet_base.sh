#!/bin/bash
# Rename a fleet in the base model
#
# Run this to fix 'bad interpreter' issue:
# sed -i -e 's/\r$//' rename_fleet_base.sh

project_path=`Rscript -e "cat(dirname(here::here()))"`
model_path=$project_path/arrowtooth-nongit/models/base

echo
echo "Renaming fleet for the base model"; \
echo
Rscript -e "gfiscamutils::rename_gear_iscam('$model_path/arf.dat', 'Wet boats', 'Shoreside')"; \
Rscript -e "gfiscamutils::rename_gear_iscam('$model_path/arf.dat', 'WB', 'SS')"; \
echo
echo "Base model fleet renamed"
echo
