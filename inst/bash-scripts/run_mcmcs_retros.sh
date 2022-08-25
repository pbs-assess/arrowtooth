#!/bin/bash
# Run retrospective model MPDs, MCMCs and MCevals in parallel
#
# Run this to fix 'bad interpreter' issue:
# sed -i -e 's/\r$//' run_mcmcs_retros.sh

 models=(01-retrospective-1-years \
         02-retrospective-2-years \
         03-retrospective-3-years \
         04-retrospective-4-years \
         05-retrospective-5-years \
         06-retrospective-6-years \
         07-retrospective-7-years \
         08-retrospective-8-years \
         09-retrospective-9-years \
         10-retrospective-10-years)

project_path=`Rscript -e "cat(gsub('(.*)/.*$', '\\\\\\1', here::here()))"`
models_path=$project_path/arrowtooth-nongit/models-mcmc-runs
base_path=$models_path/base
model_group=03-retrospective-models
chain_length=2000
save_freq=$(expr $chain_length / 2000)

for model in ${models[@]}; do
  (trap 'kill 0' SIGINT; \
   retro_year=$(echo $model | sed -n -E -e 's/^[0-9]+-retrospective-([0-9]+)-years$/\1/p')
   echo "retro year = $retro_year"; \
   echo; \
   echo "Running MPD/MCMC/MCeval for $model in a subshell"; \
   echo "Command is: iscam -retro $retro_year -mcmc $chain_length -mcsave $save_freq"; \
   echo; \
   # Create the retrospective directory if is doen't exist
   mkdir -p $models_path/$model_group/$model; \
   cd $models_path/$model_group/$model; \
   # Delete all files and directories inside recursively
   rm -rf *; \
   # Copy input files from base model
   cp $base_path/arf.ctl .; \
   cp $base_path/arf.dat .; \
   cp $base_path/arf.pfc .; \
   cp $base_path/iscam.dat .; \
   iscam -retro $retro_year; \
   mkdir -p $models_path/$model_group/$model/mcmc; \
   cp ./{arf.dat,arf.ctl,arf.pfc,iscam.dat} mcmc/; \
   cd mcmc/; \
   iscam -retro $retro_year -mcmc $chain_length -mcsave $save_freq; \
   iscam -retro $retro_year -mceval; \
   echo "$model MPD/MCMC/MCeval complete";) &
done
