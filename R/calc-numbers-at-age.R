#' Calculate the Numbers-at-age with sample sizes in a table format
#'
#' @param d A [data.frame]. Either `survey_samples` or `commercial_samples` data
#' from the [gfdata] package
#' @param survey_abbrev The survey abbreviation. See [gfdata] package
#' @param start_age The minimum age to include. This is not a minus group, it is an
#' minimum age to truncate to
#' @param plus_age The maximum age to include. This is a plus group and contains
#' the number of all ages this age and older
#' @param bysex Logical. If `TRUE` output the table by sex. Also, all records with sex
#' not equal to 1 or 2 are removed from the data
#' @param sample_type If `specimen_id`, count of `specimen_id` is the number of samples.
#' If `sample_id`, count of `sample_id` is the number of samples
#' @param old_age Age above which to remove data. Default 30
#'
#' @return A [data.frame] in tabular format with rows being years and columns, ages.
#' A new column called `nsamp` is added. It holds the number of samples all the ages
#' in each column came from
#' @importFrom dplyr filter group_by summarize ungroup n_distinct mutate_all
#' @importFrom dplyr left_join mutate transmute select everything rowwise
#' @importFrom dplyr arrange pull quo %>%
#' @importFrom tidyselect all_of
#' @importFrom purrr map flatten map_chr set_names
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom reshape2 dcast
#' @export
calc_naa <- function(d = NULL,
                     survey_abbrev = NULL,
                     start_age = 1,
                     plus_age = 20,
                     bysex = TRUE,
                     sample_type = "specimen_id",
                     old_age = 30){

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
    stopifnot(plus_age >= 1)
  }

  # Remove records without ages and over `old_age`
  d <- d |>
    filter(!is.na(age)) |>
    filter(age <= old_age)

  if(bysex){
    # Remove non-sexed fish
    d <- d |>
      filter(sex %in% 1:2)
  }else{
    # Use ALL data, by setting all sexes to the same value
    d$sex <- 1
  }

  # Sample sizes for ages
  samp_sz <- d |>
    split(~sex) |>
    map(~{
      .x |>
        group_by(year, sex) |>
        summarize(nsamp = n_distinct(!!sym(sample_type))) |>
        ungroup() |>
        select(year, nsamp, sex)
    })

  # Numbers-at-age by year with sample sizes
  d <- d |>
    split(~sex) |>
    map(~{
      .x |>
        group_by(year, age) |>
        summarize(cnt = n()) |>
        ungroup() |>
        dcast(year ~ age, value.var = "cnt") |>
        as_tibble()
    })

  naa <- map2(d, samp_sz, ~{
    j <- .x |>
      left_join(.y, by = "year") |>
      select(year, nsamp, sex, everything()) %>%
      replace(is.na(.), 0)

    if(!is.null(plus_age)){
      k <- j %>%
        select(-c(year, nsamp, sex))
      cols_in_plus_grp <- as.numeric(names(k)) >= plus_age
      k_not_in_plus_grp_df <- subset(k, select = !cols_in_plus_grp)
      k_in_plus_grp_df <- subset(k, select = cols_in_plus_grp) %>%
        mutate(rsum = rowSums(.)) |>
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
      as_tibble(k) |>
        cbind(year = j$year, nsamp = j$nsamp, sex = j$sex) |>
        select(year, nsamp, sex, everything()) |>
        mutate(year = as.integer(year)) |>
        as_tibble()
    }
  }) |>
    map_df(~{.x})

  naa
}

#' Calculate the proportions-at-age for a [data.frame] output by
#' the [calc_naa()] function
#'
#' @param naa A [data.frame] output by the [calc_naa()] function
#'
#' @return A [data.frame] in the same format as the input `naa`, but
#' proportions of ages in each year. Each row will sum to 1.
#' @export
calc_paa <- function(naa = NULL){

  stopifnot(!is.null(naa))
  stopifnot("data.frame" %in% class(naa))
  nm <- names(naa)
  stopifnot("year" %in% nm)
  stopifnot("nsamp" %in% nm)
  stopifnot("sex" %in% nm)

  j <- naa |>
    split(~ year) |>
    map_dbl(~{
      .x |> summarize(nsamp = sum(nsamp)) |> pull()
    }) |>
    enframe() |>
    rename(year = name) |>
    mutate(year = as.integer(year))

  # Add total sample size for both sexes to the table to divide by
  # so that male + female proportions = 1
  naa |>
    left_join(j, by = "year") |>
    select(year, nsamp, sex, value, everything()) |>
    mutate_at(vars(-c(year, nsamp, sex, value)), ~{.x / value}) |>
    select(-value)
}

#' Fill in a proportions-at-age table with columns required by ISCAM
#' so that it can be cut/paste directly into the ISCAM data file
#'
#' @param paa Output from [calc_paa()]
#' @param gear_num The gear number as required in the ISCAM data file
#'
#' @return a [data.frame]
#' @export
#' @examples
#' \dontrun{
#' calc_iscam_paa(calc_paa(calc_naa(comm_ft)), gear_num = 1)
#' calc_iscam_paa(calc_paa(calc_naa(comm_ss)), gear_num = 2)
#' calc_iscam_paa(calc_paa(calc_naa(survey_samples, "SYN QCS")), gear_num = 3)
#' calc_iscam_paa(calc_paa(calc_naa(survey_samples, "SYN HS")), gear_num = 5)
#' calc_iscam_paa(calc_paa(calc_naa(survey_samples, "SYN WCVI")), gear_num = 6)
#' }
calc_iscam_paa <- function(paa, gear_num = 1){

  first_cols <- paa |>
    select(year, nsamp)

  last_cols <- paa |>
    select(-year, -nsamp)

  nr <- nrow(paa)
  iscam_cols <- tibble(gear = rep(gear_num, nr),
                       area = rep(1, nr),
                       group = rep(1, nr))
  first_cols |>
    bind_cols(iscam_cols) |>
    bind_cols(last_cols)

}

#' Expand the `d` [data.frame] to include the values found in `vals`
#'
#' @details If `vals` contains any values that are not in the data frame `d`,
#' column `colname`, one new row will be added for each of them. If all the
#' values in `vals` are already in the data frame, `d` will be returned.
#' NAs will be inserted for non-value columns
#'
#' @param d A [data.frame]
#' @param vals A vector of values to expand the `d` data frame to.
#' @param colname The quoted name of a column in the [data.frame] `d`
#'
#' @return A [data.frame] of the same structure as `d` with possibly more rows
#' @export
expand_df_by_col <- function(d = NULL,
                             vals = NULL,
                             colname = NULL){
  stopifnot(!is.null(d))
  stopifnot("data.frame" %in% class(d))
  stopifnot(ncol(d) > 0)

  stopifnot(!is.null(vals))

  stopifnot(!is.null(colname))
  stopifnot(colname %in% names(d))
  quo_colname <- quo(colname)
  stopifnot(d %>% select(all_of(!!quo_colname)) %>% pull %>% class == vals %>% class)

  # Reserve column order of table
  ord <- names(d)

  # Place the colname column in the first position
  d <- d %>% select(all_of(!!quo_colname), everything())
  ncols <- ncol(d)
  missing_vals <- vals[!vals %in% (d %>% select(all_of(!!quo_colname)) %>% pull)]
  map(missing_vals, ~{
    d <<- rbind(d, c(.x, rep(NA, ncols - 1)))
  })
  d %>%
    arrange(year) %>%
    .[, ord]
}

