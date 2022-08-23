#!/bin/bash
# Run this to fix 'bad interpreter issue:
# sed -i -e 's/\r$//' run_mpds.sh

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
         13-stitched-index)

models_path=/mnt/d/github/pbs-assess/arrowtooth-project/arrowtooth-nongit/test-models-input-files-new
model_group=02-sens-models

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
  echo "Running MPD for $model in a subshell"; \
  cd $models_path/$model_group/$model; \
  clean_admb; \
  iscam; \
  echo "$model MPD complete") &

done
