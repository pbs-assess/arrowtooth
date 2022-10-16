#!/bin/bash
# Run this to fix 'bad interpreter issue:
# sed -i -e 's/\r$//' run_mpds.sh

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

models_path=/mnt/d/github/pbs-assess/arrowtooth-project/arrowtooth-nongit/models
model_group=02-sens-models

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Running MPD for $model in a subshell"; \
  cd $models_path/$model_group/$model; \
  clean_admb; \
  iscam; \
  echo "$model MPD complete") &

done
