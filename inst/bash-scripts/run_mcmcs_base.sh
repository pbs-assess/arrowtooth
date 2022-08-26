#!/bin/bash
# Run single model MPD, MCMC and MCeval in subshell
#
# Run this to fix 'bad interpreter' issue:
# sed -i -e 's/\r$//' run_mcmcs_base.sh

project_path=`Rscript -e "cat(here::here())"`
model_path=$project_path/arrowtooth-nongit/models-mcmc-runs/base
chain_length=10000000
save_freq=$(expr $chain_length / 2000)

if [ -d "$model_path" ]; then
   echo "'$model_path' found"
else
   echo "Warning: '$model_path' not found, aborting."
   exit
fi

(trap 'kill 0' SIGINT; \
 echo; \
 echo "Running MPD/MCMC/MCeval for base model in a subshell"; \
 echo "Command is: iscam -mcmc $chain_length -mcsave $save_freq"; \
 echo; \
 cd $model_path; \
 iscam; \
 rm -rf mcmc/; \
 mkdir mcmc/; \
 cp ./{arf.dat,arf.ctl,arf.pfc,iscam.dat} mcmc/; \
 cd mcmc/; \
 iscam -mcmc $chain_length -mcsave $save_freq; \
 iscam -mceval; \
 echo "Base model MCMC/MCeval complete";) &
