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




