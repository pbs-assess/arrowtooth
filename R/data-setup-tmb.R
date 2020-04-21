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
  filter(survey_abbrev == "MSSM QCS")
s_hs <- dat$survey_index %>%
  filter(survey_abbrev == "SYN HS")
s_wcvi <- dat$survey_index %>%
  filter(survey_abbrev == "SYN WCVI")

# Sample sizes for ages
samp_sz <- dat$commercial_samples %>%
  filter(!is.na(age)) %>%
  group_by(year) %>%
  summarize(nsamp = n_distinct(sample_id)) %>%
  ungroup()

# Numbers-at-age by year with sample sizes
naa <- dat$commercial_samples %>%
  filter(!is.na(age)) %>%
  group_by(year, age) %>%
  summarize(cnt = n()) %>%
  ungroup() %>%
  reshape2::dcast(year ~ age, value.var = "cnt") %>%
  as_tibble() %>%
  left_join(samp_sz, by = "year") %>%
  select(year, nsamp, everything()) %>%
  replace(is.na(.), 0)

# Proportions-at-age by year with sample sizes
paa <- naa %>%
  select(-c(year, nsamp)) %>%
  mutate(rsum = rowSums(.)) %>%
  rowwise() %>%
  mutate_all(~./rsum) %>%
  cbind(., year = naa$year, nsamp = naa$nsamp) %>%
  as_tibble() %>%
  select(year, nsamp, everything())

