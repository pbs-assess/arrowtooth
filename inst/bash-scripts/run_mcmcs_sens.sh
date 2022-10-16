#!/bin/bash
# Run sensitivity model MPDs, MCMCs and MCevals in parallel
#
# Run this to fix 'bad interpreter' issue:
# sed -i -e 's/\r$//' run_mcmcs_sens.sh

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
