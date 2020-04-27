#' Run the Statistical Catch-at-Age (SCA) model from the [MSEtool] package
#' for Arrowtooth flounder
#'
#' @return Opend an HTML page in your browser showing a summary of the model
#' outputs
#' @importFrom graphics plot
#' @importFrom methods new
#' @importFrom MSEtool SCA
#' @importFrom gfplot fit_length_weight
#' @export
run_af_sca <- function(main_dirs = set_dirs()){

  nongit_dir <- main_dirs$nongit_dir

  dat <- readRDS(file.path(nongit_dir, "data", "arrowtooth-flounder-september-2019.rds"))
  cpue <- readRDS(file.path(nongit_dir, "data", "arrowtooth-flounder-bottom-trawl-cpue-april-2020.rds"))

  # Age plus group
  plus_grp <- 20

  # Catch data
  catch <- dat$catch %>%
    group_by(year) %>%
    summarize(ct = sum(landed_kg + discarded_kg) / 1e6) %>%
    ungroup() %>%
    mutate(year = as.integer(year))

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
    expand_df_by_col(catch$year, "year")
  naa_qcs <- calc_naa(dat$survey_samples,
                      survey_abbrev = "SYN QCS",
                      plus_age = plus_grp) %>%
    expand_df_by_col(catch$year, "year")
  # There are no survey samplkes for HSMSA
  # naa_hsm <- calc_naa(dat$survey_samples,
  #                     survey_abbrev = "OTHER HS MSA",
  #                     plus_age = plus_grp) %>%
  #   expand_df_by_col(catch$year, "year")
  naa_hs <- calc_naa(dat$survey_samples,
                     survey_abbrev = "SYN HS",
                     plus_age = plus_grp) %>%
    expand_df_by_col(catch$year, "year")
  naa_wcvi <- calc_naa(dat$survey_samples,
                       survey_abbrev = "SYN WCVI",
                       plus_age = plus_grp) %>%
    expand_df_by_col(catch$year, "year")

  paa <- calc_paa(naa)
  paa_qcs <- calc_paa(naa_qcs)
  #paa_hsm <- calc_paa(naa_hsm)
  paa_hs <- calc_paa(naa_hs)
  paa_wcvi <- calc_paa(naa_wcvi)

  # Make the Data object
  af_data <- new("Data")
  af_data@Name <- "Arrowtooth Flounder"
  # Catch (must have same years as numbers-at-age or the SCA will fail)
  af_data@Cat <- catch %>% select(ct) %>% t()
  attr(af_data@Cat, "dimnames") <- NULL
  af_data@Year <- catch %>% pull(year)
  af_data@Ind <- af_data@Cat / 492.062
  af_data@Units <- "Thousand metric tonnes"
  af_data@Rec <- rep(NA, ncol(af_data@Cat)) %>% t()
  af_data@AvC <- mean(af_data@Cat)
  af_data@t <- ncol(af_data@Cat)
  # Catch-at-age (3-dimensional array)
  af_data@MaxAge <- plus_grp
  kk <- naa %>% select(-c(year, nsamp))
  arr_dim <- c(1, nrow(kk), ncol(kk))
  kk <- kk %>% unlist()
  attr(kk, "names") <- NULL
  attr(kk, "dim") <- arr_dim
  af_data@CAA <- kk
  # Mean length time series
  af_data@ML <- rep(NA, ncol(af_data@Cat)) %>% t()
  af_data@Lbar <- rep(NA, ncol(af_data@Cat)) %>% t()
  af_data@Lc <- rep(NA, ncol(af_data@Cat)) %>% t()
  # Depletion
  af_data@Dt <- 0.5
  af_data@Dep <- 0.5
  # Growth
  # Need to filter surveys here. Right now its all of them
  vbm <- calc_vb(dat$survey_samples, sex = "female")
  af_data@vbK <- vbm %>% dplyr::filter(param_name == "k") %>% pull(estimate)
  af_data@vbt0 <- vbm %>% dplyr::filter(param_name == "t0") %>% pull(estimate)
  af_data@vbLinf <- vbm %>% dplyr::filter(param_name == "linf") %>% pull(estimate)
  af_data@Mort <- 0.314
  af_data@Abun <- 294.436
  # FMSY/M (Use F in 2014 - 0.136/0.314)
  af_data@FMSY_M <- 0.43
  # Need to filter surveys here. Right now its all of them
  mat <- calc_maturity(dat$survey_samples, type = "length")
  # Following two should be single value..
  af_data@L50 <- round(c(-2, 2) * mat$se_l50 + mat$f.p0.5, 1)[1]
  af_data@L95 <- round(c(-2, 2) * mat$se_l50_95 + (mat$f.p0.95 - mat$f.p0.5), 1)[1]
  # BMSY/B0 = 119.120/492.062
  af_data@BMSY_B0 <- 0.2421
  af_data@steep <- 0.917158
  # Length-weight model
  # Need to filter surveys here. Right now its all of them
  lwm <- fit_length_weight(dat$survey_samples, sex = "female")
  af_data@wla <- exp(lwm$pars[["log_a"]])
  af_data@wlb <- lwm$pars[["b"]]

  af_sca <- SCA(Data = af_data, early_dev = "all", start = list(R0 = 1))

  # Generates the Assessment report and displays it in your browser
  report(af_sca)
}