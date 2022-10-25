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


catch_ft <- arrowtooth::extract_fleet_catch(dat$catch)
comm_ft <- arrowtooth::extract_fleet_samples(comm_samples_area_filtered)
x <- arrowtooth::export_age_comps(catch_ft, comm_ft, write_to_file = FALSE)

x <- gfplot::tidy_ages_raw(comm_samples_area_filtered, sample_type = "commercial")


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
