library(tidyverse)
source("R/calc-numbers-at-age.R")
source("R/calc-maturity.R")
source("R/length-model.R")

dat <- readRDS(here::here("data", "arrowtooth-flounder-september-2019.rds"))

# Age plus group
plus_grp <- 20
# Asymptotic length (linf)
linf <- 60.9007
# Brody growth coefficient (k)
k <- 0.2108
# Theoretical age at zero length (tt0)
tt0 <- -0.1551
# Scalar in length-weight allometry (alpha)
alpha <- 0.0053
# Power parameter in length-weight allometry (beta)
beta <- 3.1314
# Age at 50% maturity
a50 <- 5.5582
# Standard deviation at 50% maturity
sd50 <- 1.0781

# Catch data
catch <- dat$catch %>%
  group_by(year) %>%
  summarize(ct = sum(landed_kg + discarded_kg) / 1e6) %>%
  ungroup()

# Survey data
surv_abbrev <- unique(dat$survey_index$survey_abbrev)
s_qcs <- dat$survey_index %>%
  filter(survey_abbrev == "SYN QCS")
s_hsma <- dat$survey_index %>%
  filter(survey_abbrev == "OTHER HS MSA")
s_hs <- dat$survey_index %>%
  filter(survey_abbrev == "SYN HS")
s_wcvi <- dat$survey_index %>%
  filter(survey_abbrev == "SYN WCVI")

naa <- calc_naa(dat$commercial_samples, plus_age = plus_grp)
naa_qcs <- calc_naa(dat$survey_samples, survey_abbrev = "SYN QCS", plus_age = plus_grp)
naa_hsm <- calc_naa(dat$survey_samples, survey_abbrev = "OTHER HS MSA", plus_age = plus_grp)
naa_hs <- calc_naa(dat$survey_samples, survey_abbrev = "SYN HS", plus_age = plus_grp)
naa_wcvi <- calc_naa(dat$survey_samples, survey_abbrev = "SYN WCVI", plus_age = plus_grp)

paa <- calc_paa(naa)
paa_qcs <- calc_paa(naa_qcs)
paa_hsm <- calc_paa(naa_hsm)
paa_hs <- calc_paa(naa_hs)
paa_wcvi <- calc_paa(naa_wcvi)

af_dat <- DLMtool::Sole
af_dat@Name <- "Arrowtooth Flounder"
af_dat@Common_Name <- "Turbot"
af_dat@Species <- "Atheresthes stomias"
af_dat@maxage <- plus_grp
af_dat@R0 <- c(2200, 20140)
af_dat@M <- c(0.255, 0.380)
af_dat@Mexp <- c(NA_real_, NA_real_)
af_dat@Msd <- c(0.0, 0.1)
af_dat@h <- c(0.688, 0.975)
af_dat@SRrel <- 1L
af_dat@Perr <- c(0.6, 1.0)
af_dat@AC <- c(0.0, 0.7)
# Need to filter surveys here. Right now its all of them
vbm <- calc_vb(dat$survey_samples, sex = "female")
af_dat@linf <- get_cvs(vmb, param_name = "linf")
af_dat@k <- get_cvs(vmb, param_name = "k")
af_dat@t0 <- get_cvs(vmb, param_name = "t0")
log_sigma_est <- vbm %>% filter(param_name == "log_sigma") %>% pull(estimate)
af_dat@LenCV <- sqrt(exp(log_sigma_est^2) - 1)
af_dat@Ksd <- c(0.0, 0.2)
af_dat@Linfsd <- c(0.0, 0.2)
# Maturity ogive calculation
# Need to filter surveys here. Right now its all of them
mat <- calc_maturity(dat$survey_samples, type = "length")
af_dat@L50 <- round(c(-2, 2) * mat$se_l50 + mat$f.p0.5, 1)
af_dat@L50_95 <- round(c(-2, 2) * mat$se_l50_95 + (mat$f.p0.95 - mat$f.p0.5), 1)
# From 2015 assessment
af_dat@D <- c(0.367, 0.936)
lwm <- gfplot::fit_length_weight(dat$survey_samples, sex = "female")
af_dat@a <- exp(lwm$pars[["log_a"]])
af_dat@b <- lwm$pars[["b"]]
#gfplot::plot_length_weight(object_all = lwm, col = c("All" = "black"))
af_dat@Size_area_1 <- c(0.49, 0.51)
af_dat@Frac_area_1 <- c(0.49, 0.51)
af_dat@Prob_staying <- c(0.49, 0.51)
af_dat@Fdisc <- 0.99

af_fleet <- DLMtool::Generic_Fleet
af_fleet@Name <- "BC Trawl Fleet"
af_fleet@nyears <- nrow(catch)
