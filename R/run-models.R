#' Run gfiscam models
#'
#' @return Nothing
#' @importFrom gfiscamutils run_retro
#' @export
run_models <- function(){
  ## Run MLEs
  gfiscamutils::run_multiple_iscam(unique_models_dirs_full)

  ## Run MCMCs
  gfiscamutils::run_multiple_iscam(unique_models_dirs_full,
                                   mcmc_mode = TRUE,
                                   mcmc_dir = "mcmc",
                                   mcmc_chain_length = mcmc_chain_length,
                                   mcsave = mcsave,
                                   mcscale = mcscale,
                                   maxfn = maxfn,
                                   crit = crit,
                                   nox = nox)
  run_retro(base_model_dir_full,
            yrs = 1:5,
            overwrite = TRUE)
}