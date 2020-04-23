#' Calculate the Numbers-at-age with sample sizes in a table format
#'
#' @param d A [data.frame]. Either `survey_samples` or `commercial_sampels` data
#' from the [gfdata] package
#' @param survey_abbrev The survey abbreviation. See [gfdata] package
#' @param start_age The minimum age to include. This is not a minus group, it is an
#' minimum age to truncate to
#' @param plus_age The maximum age to include. This is a plus group and contains
#' the number of all ages this age and older
#'
#' @return A [data.frame] in tabular format with rows being years and columns, ages.
#' A new column called `nsamp` is added. It holds the number of samples all the ages
#' in each column came from
#' @export
calc_naa <- function(d = NULL,
                     survey_abbrev = NULL,
                     start_age = 1,
                     plus_age = NULL){
  stopifnot(!is.null(d))

  if(!is.null(survey_abbrev)){
    stopifnot(class(survey_abbrev) == "character")
    stopifnot(length(survey_abbrev) == 1)
    abb <- survey_abbrev
    d <- d %>%
      filter(survey_abbrev == abb)
  }

  stopifnot(!is.null(start_age))
  stopifnot(class(start_age) == "numeric")
  stopifnot(length(start_age) == 1)
  stopifnot(start_age >= 1)

  if(!is.null(plus_age)){
    stopifnot(class(plus_age) == "numeric")
    stopifnot(length(plus_age) == 1)
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
    select(year, nsamp, everything()) %>%
    mutate(year = as.integer(year))
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

#' Expand the `df` [data.frame] to include the values found in `vals`
#'
#' @details If `vals` contains any values that are not in the data frame `df`,
#' column `colname`, one new row will be added for each of them. If all the
#' values in `vals` are already in the data frame, `df` will be returned.
#' NAs will be inserted for non-value columns
#'
#' @param df A [data.frame]
#' @param vals A vector of values to expand the `df` data frame to.
#' @param colname The quoted name of a column in the [data.frame] `df`
#'
#' @return A [data.frame] of the same structure as `df` with possibly more rows
#' @export
expand_df_by_col <- function(df = NULL,
                             vals = NULL,
                             colname = NULL){
  stopifnot(!is.null(df))
  stopifnot(!is.null(vals))
  stopifnot(!is.null(colname))
  stopifnot("data.frame" %in%  class(df))
  stopifnot(ncol(df) > 0)
  stopifnot(colname %in% names(df))
  quo_colname <- quo(colname)
  stopifnot(df %>% select(all_of(!!quo_colname)) %>% pull %>% class == vals %>% class)

  # Reserve column order of table
  ord <- names(df)

  # Place the colname column in the first position
  df <- df %>% select(all_of(!!quo_colname), everything())
  ncols <- ncol(df)
  missing_vals <- vals[!vals %in% (df %>% select(all_of(!!quo_colname)) %>% pull)]
  map(missing_vals, ~{
    df <<- rbind(df, c(.x, rep(NA, ncols - 1)))
  })
  df %>%
    arrange(year) %>%
    .[, ord]
}

