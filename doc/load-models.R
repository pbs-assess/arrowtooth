# Make sure to set up the names for all of the models in
# `doc/models-pretty-names.R`
if(!exists("bridge_models_dirs") || !exists("bridge_models_text")){
  stop("You must source doc/models-names.R to set up the bridge ",
       "model names and directories  before sourcing doc/load-models.R")
}
if(!exists("sens_models_dirs") || !exists("sens_models_text")){
  stop("You must source doc/models-names.R to set up the sensitivity ",
       "model names and directories  before sourcing doc/load-models.R")
}
if(!exists("retro_models_dirs") || !exists("retro_models_text")){
  stop("You must source doc/models-names.R to set up the retrospective ",
       "model names and directories before sourcing doc/load-models.R")
}

if(is.null(rmarkdown::metadata$model_path)){
  warning("`model_path` not found in the `index.Rmd` file, using ",
          "defaults found at the top of `doc/load_models.R`")
  models_dir <- "/srv/arrowtooth/2022"
}else{
  models_dir <- rmarkdown::metadata$model_path
}
if(is.null(rmarkdown::metadata$base_model_path)){
  warning("`base_model_path` not found in the `index.Rmd` file, using ",
          "defaults found at the top of `doc/load_models.R`")
  base_model_dir <- "/srv/arrowtooth/2024/01-base-models/01-base-model"
}else{
  base_model_dir <- rmarkdown::metadata$base_model_path
}
if(is.null(rmarkdown::metadata$nongit_path)){
  warning("`nongit_path` not found in the `index.Rmd` file, using ",
          "defaults found at the top of `doc/load_models.R`")
  nongit_dir <- "/srv/arrowtooth/arrowtooth-nongit"
}else{
  nongit_dir <- rmarkdown::metadata$nongit_path
}

drs <- set_dirs(models_dir = models_dir,
                nongit_dir = nongit_dir,
                bridge_models_dirs = bridge_models_dirs,
                sens_models_dirs = sens_models_dirs,
                retro_models_dirs = retro_models_dirs,
                base_model_dir = base_model_dir)

models <- model_setup(drs = drs,
                      bridge_models_desc = bridge_models_text,
                      sens_models_desc = sens_models_text,
                      retro_models_desc = retro_models_text,
                      prepend_to_bridge = FALSE,
                      overwrite_rds_files = build_rds)
