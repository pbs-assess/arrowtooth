#!/bin/bash
# Run bridge model MPDs, MCMCs and MCevals in parallel
#
# Run this to fix 'bad interpreter' issue:
# sed -i -e 's/\r$//' run_mcmcs_bridge.sh

 models=(01-base-2015 \
         02-bridge-update-data-to-2014 \
         03-bridge-update-data-to-2021 \
         04-bridge-add-wchg \
         05-bridge-switch-to-dm-likelihood \
         06-bridge-switch-to-2-fleet-model \
         07-bridge-add-discard-cpue \
         08-bridge-switch-to-split-sex \
         09-bridge-switch-fishing-year-to-feb-21-feb-20 \
         10-bridge-remove-wchg \
         11-bridge-fix-natural-mortalities)

project_path=`Rscript -e "cat(dirname(here::here()))"`
models_path=$project_path/arrowtooth-nongit/models
model_group=01-bridge-models
chain_length=10000000
save_freq=$(expr $chain_length / 2000)

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
   echo; \
   echo "Running MPD/MCMC/MCeval for $model in a subshell"; \
   echo "Command is: iscam -mcmc $chain_length -mcsave $save_freq"; \
   echo; \
   cd $models_path/$model_group/$model; \
   iscam; \
   rm -rf mcmc/; \
   mkdir mcmc/; \
   cp ./{arf.dat,arf.ctl,arf.pfc,iscam.dat} mcmc/; \
   cd mcmc/; \
   iscam -mcmc $chain_length -mcsave $save_freq; \
   iscam -mceval; \
   echo "$model MPD/MCMC/MCeval complete") &
done
