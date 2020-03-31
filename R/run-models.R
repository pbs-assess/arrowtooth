source(here::here("R", "model-setup.R"))

## Run MLEs
run_multiple_iscam(unique_models_dirs_full)

## Run MCMCs
run_multiple_iscam(unique_models_dirs_full,
                   mcmc_mode = TRUE,
                   mcmc_dir = "mcmc",
                   mcmc_chain_length = 2000,
                   mcsave = 1,
                   mcscale = 0,
                   maxfn = 2000,
                   crit = 0.0001,
                   nox = TRUE)

