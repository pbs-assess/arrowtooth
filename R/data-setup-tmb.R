library(tibble)
library(dplyr)
library(purrr)
library(DLMtool)
library(MSEtool)
library(gfplot)
source("R/calc-numbers-at-age.R")
source("R/calc-maturity.R")
source("R/length-model.R")

dat <- readRDS(here::here("data", "arrowtooth-flounder-september-2019.rds"))

# Age plus group
plus_grp <- 20

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

naa <- calc_naa(dat$commercial_samples,
                plus_age = plus_grp) %>%
  fill_naa_years(catch$year)
naa_qcs <- calc_naa(dat$survey_samples,
                    survey_abbrev = "SYN QCS",
                    plus_age = plus_grp) %>%
  fill_naa_years(catch$year)
naa_hsm <- calc_naa(dat$survey_samples,
                    survey_abbrev = "OTHER HS MSA",
                    plus_age = plus_grp) %>%
  fill_naa_years(catch$year)
naa_hs <- calc_naa(dat$survey_samples,
                   survey_abbrev = "SYN HS",
                   plus_age = plus_grp) %>%
  fill_naa_years(catch$year)
naa_wcvi <- calc_naa(dat$survey_samples,
                     survey_abbrev = "SYN WCVI",
                     plus_age = plus_grp) %>%
  fill_naa_years(catch$year)

paa <- calc_paa(naa)
paa_qcs <- calc_paa(naa_qcs)
paa_hsm <- calc_paa(naa_hsm)
paa_hs <- calc_paa(naa_hs)
paa_wcvi <- calc_paa(naa_wcvi)

# Make the Data object
xx <- new("Data")
xx@Name <- "Arrowtooth Flounder"
# Catch (must have same years as numbers-at-age or the SCA will fail)
#catch <- catch %>% dplyr::filter(year %in% naa$year)
xx@Cat <- catch %>% select(ct) %>% t()
attr(xx@Cat, "dimnames") <- NULL
xx@Year <- catch %>% pull(year)
xx@Ind <- xx@Cat / 492.062
xx@Units <- "Thousand metric tonnes"
xx@Rec <- rep(NA, ncol(xx@Cat)) %>% t()
xx@AvC <- mean(xx@Cat)
xx@t <- ncol(xx@Cat)
# Catch-at-age (3-dimensional array)
xx@MaxAge <- plus_grp
kk <- naa %>% select(-c(year, nsamp))
arr_dim <- c(1, ncol(kk), nrow(kk))
kk <- kk %>% unlist()
attr(kk, "names") <- NULL
attr(kk, "dim") <- arr_dim
xx@CAA <- kk
# Mean length time series
xx@ML <- rep(NA, ncol(xx@Cat)) %>% t()
xx@Lbar <- rep(NA, ncol(xx@Cat)) %>% t()
xx@Lc <- rep(NA, ncol(xx@Cat)) %>% t()
# Depletion
xx@Dt <- 0.5
xx@Dep <- 0.5
# Growth
# Need to filter surveys here. Right now its all of them
vbm <- calc_vb(dat$survey_samples, sex = "female")
xx@vbK <- vbm %>% dplyr::filter(param_name == "k") %>% pull(estimate)
xx@vbt0 <- vbm %>% dplyr::filter(param_name == "t0") %>% pull(estimate)
xx@vbLinf <- vbm %>% dplyr::filter(param_name == "linf") %>% pull(estimate)
xx@Mort <- 0.314
xx@Abun <- 294.436
# FMSY/M (Use F in 2014 - 0.136/0.314)
xx@FMSY_M <- 0.43
# Need to filter surveys here. Right now its all of them
mat <- calc_maturity(dat$survey_samples, type = "length")
# Following two should be single value..
xx@L50 <- round(c(-2, 2) * mat$se_l50 + mat$f.p0.5, 1)[1]
xx@L95 <- round(c(-2, 2) * mat$se_l50_95 + (mat$f.p0.95 - mat$f.p0.5), 1)[1]
# BMSY/B0 = 119.120/492.062
xx@BMSY_B0 <- 0.2421

j <- SCA(Data = xx, early_dev = "all", start = list(R0 = 1))


# This is for the Stock object (xx)
# xx@Name <- "Arrowtooth Flounder"
# xx@Common_Name <- "Turbot"
# xx@Species <- "Atheresthes stomias"
# xx@maxage <- plus_grp
# xx@R0 <- c(2200, 20140)
# xx@M <- c(0.255, 0.380)
# xx@Mexp <- c(NA_real_, NA_real_)
# xx@Msd <- c(0.0, 0.1)
# xx@h <- c(0.688, 0.975)
# xx@SRrel <- 1L
# xx@Perr <- c(0.6, 1.0)
# xx@AC <- c(0.0, 0.7)
# # Need to filter surveys here. Right now its all of them
# vbm <- calc_vb(dat$survey_samples, sex = "female")
# xx@linf <- get_cvs(vmb, param_name = "linf")
# xx@k <- get_cvs(vmb, param_name = "k")
# xx@t0 <- get_cvs(vmb, param_name = "t0")
# log_sigma_est <- vbm %>% filter(param_name == "log_sigma") %>% pull(estimate)
# xx@LenCV <- sqrt(exp(log_sigma_est^2) - 1)
# xx@Ksd <- c(0.0, 0.2)
# xx@Linfsd <- c(0.0, 0.2)
# # Maturity ogive calculation
# # Need to filter surveys here. Right now its all of them
# mat <- calc_maturity(dat$survey_samples, type = "length")
# xx@L50 <- round(c(-2, 2) * mat$se_l50 + mat$f.p0.5, 1)
# xx@L50_95 <- round(c(-2, 2) * mat$se_l50_95 + (mat$f.p0.95 - mat$f.p0.5), 1)
# # From 2015 assessment
# xx@D <- c(0.367, 0.936)
# lwm <- gfplot::fit_length_weight(dat$survey_samples, sex = "female")
# xx@a <- exp(lwm$pars[["log_a"]])
# xx@b <- lwm$pars[["b"]]
# #gfplot::plot_length_weight(object_all = lwm, col = c("All" = "black"))
# xx@Size_area_1 <- c(0.49, 0.51)
# xx@Frac_area_1 <- c(0.49, 0.51)
# xx@Prob_staying <- c(0.49, 0.51)
# xx@Fdisc <- 0.99


# af_fleet <- DLMtool::Generic_Fleet
# af_fleet@Name <- "BC Trawl Fleet"
# af_fleet@nyears <- nrow(catch)
