calc_naa <- function(d, survey_abbrev = NULL, start_age = 1, plus_age = NULL){

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
  d <- d %>%
    filter(!is.na(age)) %>%
    group_by(year, age) %>%
    summarize(cnt = n()) %>%
    ungroup() %>%
    reshape2::dcast(year ~ age, value.var = "cnt") %>%
    as_tibble() %>%
    left_join(samp_sz, by = "year") %>%
    select(year, nsamp, everything()) %>%
    replace(is.na(.), 0)

  if(is.null(plus_age)){
    return(d)
  }
  k <- d %>%
    select(-c(year, nsamp))
  cols_in_plus_grp <- as.numeric(names(k)) >= plus_age
  k_not_in_plus_grp_df <- subset(k, select = !cols_in_plus_grp)
  k_in_plus_grp_df <- subset(k, select = cols_in_plus_grp) %>%
    mutate(rsum = rowSums(.)) %>%
    transmute(rsum)
  k <- cbind(k_not_in_plus_grp_df, k_in_plus_grp_df)
  names(k)[ncol(k)] <- plus_age
  ages <- start_age:plus_age
  missing_ages <- ages[!ages %in% names(k)]
  if(length(missing_ages)){
    # Create a new column for each missing age, bind to data frame and sort columns by age
    map(missing_ages, ~{
      #browser()
      new_colname <- as.character(.x)
      k <<- k %>% mutate(!!new_colname := 0)
    })
    k <- k %>% select(as.character(ages))
  }
  # Remove ages less than start_age
  age_cols_to_keep <- start_age <= as.numeric(names(k))
  k <- subset(k, select = age_cols_to_keep)
  as_tibble(k) %>%
    cbind(year = d$year, nsamp = d$nsamp) %>%
    select(year, nsamp, everything())
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
    select(year, nsamp, everything()) %>%
    select(-rsum)
}
