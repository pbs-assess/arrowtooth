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

calc_naa <- function(d, survey_abbrev = NULL){

  if(!is.null(survey_abbrev)){
    d <- d %>%
      filter(survey_abbrev == survey_abbrev)
  }

  # Sample sizes for ages
  samp_sz <- d %>%
    filter(!is.na(age)) %>%
    group_by(year) %>%
    summarize(nsamp = n_distinct(sample_id)) %>%
    ungroup()

  # Numbers-at-age by year with sample sizes
  d %>%
    filter(!is.na(age)) %>%
    group_by(year, age) %>%
    summarize(cnt = n()) %>%
    ungroup() %>%
    reshape2::dcast(year ~ age, value.var = "cnt") %>%
    as_tibble() %>%
    left_join(samp_sz, by = "year") %>%
    select(year, nsamp, everything()) %>%
    replace(is.na(.), 0)
}

calc_paa <- function(naa){
  # Proportions-at-age by year with sample sizes
  naa %>%
    select(-c(year, nsamp)) %>%
    mutate(rsum = rowSums(.)) %>%
    rowwise() %>%
    mutate_all(~./rsum) %>%
    cbind(., year = naa$year, nsamp = naa$nsamp) %>%
    as_tibble() %>%
    select(year, nsamp, everything())
}

naa <- calc_naa(dat$commercial_samples)
naa_qcs <- calc_naa(dat$survey_samples, "SYN QCS")
naa_hsm <- calc_naa(dat$survey_samples, "OTHER HS MSA")
naa_hs <- calc_naa(dat$survey_samples, "SYN HS")
naa_wcvi <- calc_naa(dat$survey_samples, "SYN WCVI")

paa <- calc_paa(naa)
paa_qcs <- calc_paa(naa_qcs)
paa_hsm <- calc_paa(naa_hsm)
paa_hs <- calc_paa(naa_hs)
paa_wcvi <- calc_paa(naa_wcvi)

