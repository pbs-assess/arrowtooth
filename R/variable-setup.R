assess_yr <- 2020 #as.numeric(substr(Sys.Date(), 1, 4))
last_assess_yr <- 2015

# Age class of plus group for proportion-at-age
age_plus <- 10

# Age class to highlight in weight-at-age plots
age_highlight <- 3

# Spawn survey method changed from surface (1951--1987) to dive (1988--present)
new_surv_yr <- 1988

# Post-reduction fishery catches
recent_catch_yr <- 1972

# Limits for the weight-at-age plot
wa_ylim <- c(0.05, 0.15)

# Number of years to calculate running mean weight-at-age (window)
nRollWA <- 5

# Number of years to calculate running mean recruitment (window)
nRollRec <- 3

# Target harvest rate
target_hr <- 0.2

regions <- tribble(
  ~SAR, ~Region,                      ~RegionName, ~Major,
  1,       "HG",                    "Haida Gwaii",   TRUE,
  2,      "PRD",         "Prince Rupert District",   TRUE,
  3,       "CC",                  "Central Coast",   TRUE,
  4,      "SoG",              "Strait of Georgia",   TRUE,
  5,     "WCVI", "West Coast of Vancouver Island",   TRUE,
  6,      "A27",                        "Area 27",  FALSE,
  7,      "A2W",                    "Area 2 West",  FALSE)
regions$Region <- en2fr(regions$Region, french)
regions$RegionName <- en2fr(regions$RegionName, french)

gear <- tribble(
  ~gear,   ~gearname,
      1,     "Other",
      2,     "RoeSN",
      3,     "RoeGN")
gear$gearname <- en2fr(gear$gearname, french)
roe_gear <- c(2, 3)

# Survey gear follows the fishery gears above, but q parameters exist only for survey
# gears so they are included here as `qind`
surv_type <- tribble(
  ~gear,   ~gearname, ~qind,
      4,   "Surface",     1,
      5,      "Dive",     2)
surv_type$gearname <- en2fr(surv_type$gearname, french)

major_models <- load.models(unlist(major_model_dirs), inc_retro = TRUE)
minor_models <- load.models(unlist(minor_model_dirs), inc_retro = FALSE)
# Assumes all major region models have the same year range
major_start_yr <- major_models[[1]]$dat$start.yr
major_end_yr <- major_models[[1]]$dat$end.yr
major_yr_range <- major_start_yr:major_end_yr
# Assumes all minor region models have the same year range
minor_start_yr <- minor_models[[1]]$dat$start.yr
minor_end_yr <- minor_models[[1]]$dat$end.yr
minor_yr_range <- minor_start_yr:minor_end_yr
minor_start_yr_plot <- 1978

all_regions_short <- regions$Region
major_regions_short <- regions$Region[regions$Major]
minor_regions_short <- regions$Region[!regions$Major]

all_regions_full <- regions$RegionName
major_regions_full <- regions$RegionName[regions$Major]
minor_regions_full <- regions$RegionName[!regions$Major]

all_regions_full_parens <- paste0(all_regions_full,  " (", all_regions_short, ")")
major_regions_full_parens <- paste0(major_regions_full,  " (", major_regions_short, ")")
minor_regions_full_parens <- paste0(minor_regions_full,  " (", minor_regions_short, ")")

#Catch
major_catch <- get_catch(major_models,
                         major_regions_full,
                         gear)
major_catch_short <- get_catch(major_models,
                               major_regions_short,
                               gear)
minor_catch <- get_catch(minor_models,
                         minor_regions_full,
                         gear)
minor_catch_short <- get_catch(minor_models,
                               minor_regions_short,
                               gear)

suppressWarnings(
  ## supress warnings is because the two tables have different factors for the `region` column
  ## and are therefore coerced to character types before binding
  total_final_yr_catch <- bind_rows(major_catch_short, minor_catch_short) %>%
    filter(year == assess_yr)
)

total_final_yr_roe_catch <- total_final_yr_catch %>%
  filter(gear %in% !!gear$gearname[c(2,3)]) %>%
  summarize(catch = sum(value) * 1000) %>%
  pull() %>%
  f()

total_final_yr_other_catch <- total_final_yr_catch %>%
  filter(!gear %in% !!gear$gearname[c(2,3)]) %>%
  summarize(catch = sum(value) * 1000) %>%
  pull() %>%
  f()

#Spawn-on-kelp
sok_file_pattern <- "harvest-sok-*"
data_path <- here::here("data")
sok_filenames <- dir(data_path, pattern = sok_file_pattern)
sok <- sok_filenames %>%
  map(~read_csv(file.path(data_path, .))) %>%
  reduce(rbind)
sok$Region <- en2fr(sok$Region, french)

#Proportion-of-spawn
ps_file_pattern <- "prop-spawn-*"
data_path <- here::here("data")
ps_filenames <- dir(data_path, pattern = ps_file_pattern)
ps_shortnames <- sub("prop-spawn-", "", ps_filenames )
ps_shortnames <- toupper(sub(".csv", "", ps_shortnames))
ps_shortnames[ps_shortnames == "SOG"] <- "SoG"
ps_filenames <- file.path(data_path, ps_filenames)
ps <- lapply(ps_filenames, function(x){
  read_csv(x)})
names(ps) <- en2fr(ps_shortnames, french)

#Weight-at-age
major_wa <- get_wa(major_models,
                   major_regions_full,
                   gear)
minor_wa <- get_wa(minor_models,
                   minor_regions_full,
                   gear)
#Proportion-at-age
major_pa <- get_pa(major_models,
                   major_regions_full,
                   gear)
minor_pa <- get_pa(minor_models,
                   minor_regions_full,
                   gear)

#Survey Indices
major_surv <- get_surv_ind(major_models,
                           major_regions_full,
                           surv_type)
surv_yr_rng <- range(major_surv$year)

minor_surv <- get_surv_ind(minor_models,
                           minor_regions_full,
                           surv_type)

minor_surv_short <- get_surv_ind(minor_models,
                                 minor_regions_short,
                                 surv_type)

# Input catch for table 1
inp_catch <- read_csv(here::here("data/input-data.csv"))

#' Get model output values for a given region
#'
#' @param region the short form for the region as defined in the `major_regions_short` vector`
#' @param majors if TRUE, use major models list (`major_models`), if FALSE use minor models list (`minor_models`)
#' @param french if TRUE, use french
#'
#' @return a list of values as required for the report
#'
#' @examples
#' hg_vars <- get_vars("HG")
get_vars <- function(region, majors = TRUE, french = FALSE){
  if(majors){
    model_ind <- match(en2fr(region, french), major_regions_short)
    models <- major_models
  }else{
    model_ind <- match(en2fr(region, french), minor_regions_short)
    models <- minor_models
  }
  sbt <- models[[model_ind]]$mcmccalcs$sbt.quants
  sbt_yrs <- as.numeric(colnames(sbt))
  ## Final year spawning biomass - vector length 4 - 1 = lower, 2 = median, 3 = upper, 4 = mpd
  final_yr_sbt <- sbt[, sbt_yrs == assess_yr] * 1000
  refs <- models[[model_ind]]$mcmccalcs$r.quants
  sbo <- refs[rownames(refs) == "sbo",][2:4] * 1000
  ## Probability that final year biomass is less than 0.3B0 - vector length 3 - 1 = lower, 2 = median, 3 = upper
  prob_less_03sbo <- refs[rownames(refs) == paste0("psb", assess_yr, "/0.3sbo"),][2]
  proj <- as_tibble(models[[model_ind]]$mcmccalcs$r.quants, rownames = "value")
  ## Projected biomass for next year - vector length 3 - 1 = lower, 2 = median, 3 = upper
  proj_sbt <- as.numeric(proj[proj$value == paste0("sb", assess_yr + 1), -c(1, 2)]) * 1000
  ## Probability that next year (projected) biomass is less than 0.3B0 - vector length 3 - 1 = lower, 2 = median, 3 = upper
  prob_proj_less_03sbo <- refs[rownames(refs) == paste0("psb", assess_yr + 1, "/0.3sbo"),][2]

  list(final_yr_sbt = final_yr_sbt,
       sbo = sbo,
       prob_less_03sbo = prob_less_03sbo,
       proj_sbt = proj_sbt,
       prob_proj_less_03sbo = prob_proj_less_03sbo)
}
# -----------------------------------------------------------------------------
# Haida Gwaii-dependent values
hg_vars <- get_vars("HG", french = french)
hg_final_yr_sbt <- hg_vars[["final_yr_sbt"]]
hg_sbo <- hg_vars[["sbo"]]
hg_prob_less_03sbo <- hg_vars[["prob_less_03sbo"]]
hg_proj_sbt <- hg_vars[["proj_sbt"]]
hg_prob_proj_less_03sbo <- hg_vars[["prob_proj_less_03sbo"]]
# -----------------------------------------------------------------------------
# Prince Rupert District-dependent values
prd_vars <- get_vars("PRD", french = french)
prd_final_yr_sbt <- prd_vars[["final_yr_sbt"]]
prd_sbo <- prd_vars[["sbo"]]
prd_prob_less_03sbo <- prd_vars[["prob_less_03sbo"]]
prd_proj_sbt <- prd_vars[["proj_sbt"]]
prd_prob_proj_less_03sbo <- prd_vars[["prob_proj_less_03sbo"]]
# -----------------------------------------------------------------------------
# Central Coast-dependent values
cc_vars <- get_vars("CC", french = french)
cc_final_yr_sbt <- cc_vars[["final_yr_sbt"]]
cc_sbo <- cc_vars[["sbo"]]
cc_prob_less_03sbo <- cc_vars[["prob_less_03sbo"]]
cc_proj_sbt <- cc_vars[["proj_sbt"]]
cc_prob_proj_less_03sbo <- cc_vars[["prob_proj_less_03sbo"]]
# -----------------------------------------------------------------------------
# Strait of Georgia District-dependent values
sog_vars <- get_vars("SoG", french = french)
sog_final_yr_sbt <- sog_vars[["final_yr_sbt"]]
sog_sbo <- sog_vars[["sbo"]]
sog_prob_less_03sbo <- sog_vars[["prob_less_03sbo"]]
sog_proj_sbt <- sog_vars[["proj_sbt"]]
sog_prob_proj_less_03sbo <- sog_vars[["prob_proj_less_03sbo"]]
# -----------------------------------------------------------------------------
# West Coast of Vancouver Island-dependent values
wcvi_vars <- get_vars("WCVI", french = french)
wcvi_final_yr_sbt <- wcvi_vars[["final_yr_sbt"]]
wcvi_sbo <- wcvi_vars[["sbo"]]
wcvi_prob_less_03sbo <- wcvi_vars[["prob_less_03sbo"]]
wcvi_proj_sbt <- wcvi_vars[["proj_sbt"]]
wcvi_prob_proj_less_03sbo <- wcvi_vars[["prob_proj_less_03sbo"]]


## Number of mcmc samples, min and max median biomass
mcmc_num_samples <- nrow(major_models[[1]]$mcmc$params)
mcmc_burnin <- mcmc_num_samples - nrow(major_models[[1]]$mcmccalcs$p.dat)
mcmc_length <- "5 million"
mcmc_samp_freq <- 1000
mcmc_ci <- "90\\%"

# -----------------------------------------------------------------------------
# MP decision tables
cc_mp <- read_csv("data/mp-cc.csv")

proc_mp <- function(df){
  csas_table(df,
             format = "latex",
             bold_header = TRUE,
             col_names = c("Scenario",
                           "MP",
                           "Label",
                           "Conservation\nObj 1 (LRP)\n>75%",
                           "Biomass\nObj 2\n>50%",
                           "Yield\nObj 3\n<25%",
                           "Yield\nObj 4\nmax",
                           "Catch < 650t\nmin"))

}
