dat <- readRDS("~/src/arrowtooth-nongit/data/arrowtooth-flounder-aug11-2022.rds")

library(dplyr)

survs <- c("SYN HS", "SYN QCS", "SYN WCVI", "OTHER HS MSA")

d <- dat$survey_index |> filter(survey_abbrev %in% survs)

d$se_log <- round(sqrt(log(1 + d$re^2)), 2)

discard_cpue_file <- file.path("~/src/arrowtooth-nongit/data/",
  "cpue-predictions-arrowtooth-flounder-modern-3CD5ABCDE-discard-july-26-feb-fishing-year.csv")
dcpue <- readr::read_csv(discard_cpue_file)

dcpue <- filter(dcpue, formula_version == "Full standardization")

dcpue <- select(dcpue, year, biomass = est, se_log = se_link)
dcpue$survey_abbrev <- "DCPUE"
dcpue$month <- 7
dcpue$fleet <- 7
dcpue$biomass <- round(dcpue$biomass / 10, 2)
dcpue$se_log <- round(dcpue$se_log, 2)

# QCS
# HSM
# HS
# WCVI
# DCPUE
surv_lu <- data.frame(survey_abbrev = c("SYN QCS", "OTHER HS MSA", "SYN HS", "SYN WCVI"), fleet = c(3, 4, 5, 6), stringsAsFactors = FALSE)

out <- select(d, year, survey_abbrev, biomass, se_log)
out$month <- 7
out <- left_join(out, surv_lu)

out <- select(out, year, month, fleet, biomass, se_log, survey_abbrev)
out$biomass <- round(out$biomass / 1000000, 2)

out <- bind_rows(out, dcpue)

out <- arrange(out, fleet, year)

out$survey_abbrev <- paste0("#_ ", out$survey_abbrev)

readr::write_delim(out, "~/Desktop/arf-surv.txt", delim = " ", quote = "none")

# COMPS

# dat <- gfdata::cache_pbs_data("arrowtooth flounder")
# comm_samples_area_filtered <- dat$commercial_samples |>
#   filter(major_stat_area_code %in% c("03","04", "05", "06", "07", "08", "09"))
# catch_ft <- arrowtooth::extract_fleet_catch(dat$catch)
# comm_ft <- arrowtooth::extract_fleet_samples(comm_samples_area_filtered)
# extract_age_comps(catch_ft, comm_ft)


# catch_ft <- arrowtooth::extract_fleet_catch(dat$catch)
# comm_ft <- arrowtooth::extract_fleet_samples(comm_samples_area_filtered)
# x <- arrowtooth::export_age_comps(catch_ft, comm_ft, write_to_file = FALSE)
#
# x <- gfplot::tidy_ages_weighted(comm_samples_area_filtered, sample_type = "commercial", dat_survey_sets = )

get_age_comps_ss3 <- function(dat_list, type = c("commercial", "survey"), .survey_abbrev = NULL, freezer_trawlers = TRUE) {

  type <- match.arg(type)

  comm_samples_area_filtered <- dat_list$commercial_samples |>
    dplyr::filter(major_stat_area_code %in% c("03","04", "05", "06", "07", "08", "09"))

  if (type == "commercial") {
    samps <- arrowtooth::extract_fleet_samples(comm_samples_area_filtered,
      include = freezer_trawlers)
  } else {
    samps <- dat_list$survey_samples |> filter(survey_abbrev == .survey_abbrev)
  }

  all_lu <- expand.grid(sex = c(1, 2), year = 1996:2021, age = 1:25, n = NA,
    stringsAsFactors = FALSE)
  missing <- anti_join(all_lu, select(samps, sex, year, age))

  y <- samps %>%
    group_by(year, age, sex) %>%
    summarise(n = n()) |>
    filter(!is.na(age)) |>
    filter(age <= 25)

  all <- bind_rows(y, missing)

  yy <- tidyr::pivot_wider(all, names_from = age, values_from = n)
  yy[is.na(yy)] <- 0L
  yy <- select(yy, year, sex, as.character(seq(1, 25)))
  yf <- yy |> filter(sex == 2)
  ym <- yy |> filter(sex == 1)

  yf$month <- 7
  yf$fleet <- 1
  yf$sex <- 3
  yf$part <- 0
  yf$ageerr <- 0 #!?
  yf$`Lbin_lo` <- 1
  yf$`Lbin_hi` <- -1
  yf$Nsamp <- NA
  yf <- select(yf, year, month, fleet, sex, part, ageerr, `Lbin_lo`, `Lbin_hi`, Nsamp, as.character(1:25))
  names(yf)[10:ncol(yf)] <- paste0("F", names(yf)[10:ncol(yf)])

  ym$sex <- NULL
  ym$year <- NULL
  names(ym) <- paste0("M", names(ym))

  stopifnot(nrow(ym) == nrow(yf))

  yt <- bind_cols(yf, ym)

  yt$Nsamp <- apply(yt[,-c(1:9),], 1, sum)
  yt <- arrange(yt, year)
  yt
}

get_length_comps_ss3 <- function(dat_list, type = c("commercial", "survey"),
  .survey_abbrev = NULL, freezer_trawlers = TRUE) {

  type <- match.arg(type)

  comm_samples_area_filtered <- dat_list$commercial_samples |>
    dplyr::filter(major_stat_area_code %in% c("03","04", "05", "06", "07", "08", "09"))

  if (type == "commercial") {
    samps <- arrowtooth::extract_fleet_samples(comm_samples_area_filtered,
      include = freezer_trawlers)
  } else {
    samps <- dat_list$survey_samples |> filter(survey_abbrev == .survey_abbrev)
  }

  bin_range <- c(5, 80)
  bin_size <- 2
  bins <- seq(min(bin_range), max(bin_range), by = bin_size)

  samps$length[samps$length >= 80] <- 79.99
  samps <- filter(samps, year >= 1996)
  samps$length_bin <- bins[findInterval(samps$length, bins)]

  all_lu <- expand.grid(sex = c(1, 2), year = 1996:2021, length_bin = bins, n = NA,
    stringsAsFactors = FALSE)
  missing <- anti_join(all_lu, select(samps, sex, year, length_bin))

  y <- samps %>%
    filter(!is.na(length)) |>
    filter(sex %in% c(1, 2)) |>
    group_by(year, length_bin, sex) %>%
    summarise(n = n())

  # ggplot(y, aes(length_bin, n, fill = sex)) + facet_wrap(~year) + geom_col()
  # g <- ggplot(y, aes(length_bin, n)) + facet_grid(year~sex) + geom_col()
  # print(g)

  all <- bind_rows(y, missing)

  yy <- tidyr::pivot_wider(all, names_from = length_bin, values_from = n)
  yy[is.na(yy)] <- 0L
  yy <- select(yy, year, sex, as.character(bins))
  yf <- yy |> filter(sex == 2)
  ym <- yy |> filter(sex == 1)

  yf$month <- 7
  yf$fleet <- 1
  yf$sex <- 3
  yf$part <- 0
  yf$`Lbin_lo` <- 1
  yf$`Lbin_hi` <- -1
  yf$Nsamp <- NA
  yf <- select(yf, year, month, fleet, sex, part, Nsamp, as.character(bins))
  names(yf)[7:ncol(yf)] <- paste0("F", names(yf)[7:ncol(yf)])

  ym$sex <- NULL
  ym$year <- NULL
  names(ym) <- paste0("M", names(ym))

  stopifnot(nrow(ym) == nrow(yf))

  yt <- bind_cols(yf, ym)

  yt$Nsamp <- apply(yt[,-c(1:7),], 1, sum)
  # yt$Nsamp <- 100
  yt <- arrange(yt, year)
  yt
}

f1 <- get_length_comps_ss3(dat, type = "commercial", freezer_trawlers = TRUE)
f2 <- get_length_comps_ss3(dat, type = "commercial", freezer_trawlers = FALSE)
f2$fleet <- 2
f3 <- get_length_comps_ss3(dat, type = "survey", .survey_abbrev = "SYN QCS")
f3$fleet <- 3
f5 <- get_length_comps_ss3(dat, type = "survey", .survey_abbrev = "SYN HS")
f5$fleet <- 5
f6 <- get_length_comps_ss3(dat, type = "survey", .survey_abbrev = "SYN WCVI")
f6$fleet <- 6

all_comps <- bind_rows(list(f1, f2, f3, f5, f6))
all_comps <- filter(all_comps, Nsamp > 0)
all_comps$Nsamp <- 100

end <- c(-9999, rep(0, ncol(all_comps)-1))
names(end) <- names(all_comps)

all_comps <- bind_rows(all_comps, end)

readr::write_delim(all_comps, file = "~/Desktop/arf-length-comps.txt", delim = " ")


f1 <- get_age_comps_ss3(dat, type = "commercial", freezer_trawlers = TRUE)
f2 <- get_age_comps_ss3(dat, type = "commercial", freezer_trawlers = FALSE)
f2$fleet <- 2
f3 <- get_age_comps_ss3(dat, type = "survey", .survey_abbrev = "SYN QCS")
f3$fleet <- 3
# f4 <- get_age_comps_ss3(dat, type = "survey", .survey_abbrev = "OTHER HS MSA")
f5 <- get_age_comps_ss3(dat, type = "survey", .survey_abbrev = "SYN HS")
f5$fleet <- 5
f6 <- get_age_comps_ss3(dat, type = "survey", .survey_abbrev = "SYN WCVI")
f6$fleet <- 6
# f7 <- get_age_comps_ss3(dat, type = "survey", .survey_abbrev = "DCPUE")

all_comps <- bind_rows(list(f1, f2, f3, f5, f6))
all_comps <- filter(all_comps, Nsamp > 0)

end <- c(-9999, rep(0, ncol(all_comps)-1))
names(end) <- names(all_comps)

all_comps <- bind_rows(all_comps, end)

readr::write_delim(all_comps, file = "~/Desktop/arf-comps.txt", delim = " ")


# growth and M

mf <- dat$survey_samples |> gfplot::fit_vb(sex = "female")
mf$predictions
mf$pars

mf <- dat$survey_samples |> gfplot::fit_length_weight(sex = "female")
mf$pars
exp(mf$pars$log_a)
mf$pars$b

gfiscamutils::export_mat_lw_age(dat$survey_samples, write_file = FALSE)

mm <- dat$survey_samples |> gfplot::fit_vb(sex = "male")
mm$predictions

# output ------------------------------------------------------------------

library(r4ss)
library(dplyr)
# setwd("../ss3/arf")

# d <- SS_output(".")
# d <- SS_output("~/src/arrowtooth/ss3/arf-dome/")
# SS_plots(d, forecastplot = TRUE, effNline = TRUE)

# r4ss::run(
#   f,
#   skipfinished = FALSE, show_in_console = TRUE
# )

setwd("..")
unlink("arf-iscam-sigma", force = TRUE)
system("cp -r arf arf-iscam-sigma")
setwd("arf-iscam-sigma")

# dat <- r4ss::SS_readdat("data.ss")
# # dat$CPUE$se_log <- dat$CPUE$se_log * 0.5
#
# dat$CPUE <- dat$CPUE |>
#   group_by(index) |>
#   mutate(w = 1/se_log) |>
#   mutate(w_inv = 1/(w / mean(w))) |>
#   mutate(se_log_iscam = w_inv * 0.2) |>
#   select(year, seas, index, obs, se_log = se_log_iscam) |>
#   ungroup() |>
#   as.data.frame()

# r4ss::SS_writedat(dat, "data.ss", overwrite = TRUE)

r4ss::run(".",
  skipfinished = FALSE, show_in_console = TRUE
)

d <- SS_output(".")
SS_plots(d, forecastplot = TRUE)
SSplotTimeseries(d, subplot = 7)
SSplotTimeseries(d, subplot = 7)

#
# calc_beta_mean_sd <- function(alpha, beta) {
#   mu <- alpha/(alpha + beta)
#   sd <- sqrt(alpha * beta/((alpha + beta)^2 * (alpha + beta +
#       1)))
#   c(mean = mu, sd = sd)
# }
# calc_beta_mean_sd(13.4, 2.4)
library(ggplot2)

d$cpue |> filter(Fleet_name %in% c("DCPUE")) |>
  ggplot(aes(Yr, Vuln_bio, colour = Fleet_name)) +
  geom_line()
