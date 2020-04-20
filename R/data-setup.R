dat <- readRDS(here::here("data", "arrowtooth-flounder-september-2019.rds"))
survey_sets <- dat$survey_sets
survey_samples <- dat$survey_samples
commercial_samples <- dat$commercial_samples
catch <- dat$catch
cpue_spatial <- dat$cpue_spatial
cpue_spatial_ll <- dat$cpue_spatial_ll
survey_index <- dat$survey_index
age_precision <- dat$age_precision

# cpue <- readRDS(here::here("data", "cpue-index-bottom-trawl-april-2020.rds"))
# cpue <- cpue %>% filter(species_code == 602)
# cpue <- tidy_cpue_index(cpue, species_common = "Arrowtooth Flounder")
# saveRDS(cpue, here::here("data", "arrowtooth-flounder-bottom-trawl-cpue-april-2020.rds"))

cpue <- readRDS(here::here("data", "arrowtooth-flounder-bottom-trawl-cpue-april-2020.rds"))
#eu <- readRDS(here::here("data", "eulachon_specimens.rds"))
