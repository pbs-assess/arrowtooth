#!/bin/bash
# Run sensitivity model MPDs, MCMCs and MCevals in parallel
#
# Run this to fix 'bad interpreter' issue:
# sed -i -e 's/\r$//' run_mcmcs_sens.sh

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

project_path=`Rscript -e "cat(gsub('(.*)/.*$', '\\\\\\1', here::here()))"`
models_path=$project_path/arrowtooth-nongit/models-mcmc-runs
model_group=02-sens-models
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
   echo; \
   echo "$model MPD/MCMC/MCeval complete"; \
   echo) &
done
