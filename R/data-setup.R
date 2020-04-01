dat <- readRDS(here::here("data", "arrowtooth-flounder-2019.rds"))
survey_sets <- dat$survey_sets
survey_samples <- dat$survey_samples
commercial_samples <- dat$commercial_samples
catch <- dat$catch
cpue_spatial <- dat$cpue_spatial
cpue_spatial_ll <- dat$cpue_spatial_ll
survey_index <- dat$survey_index
age_precision <- dat$age_precision
