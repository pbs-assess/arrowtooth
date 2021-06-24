#' Estimate the length/weight parameters for the dataset `d`. Columns `weight`
#' and `length` are required
#'
#' @param d A data frame returned by running [gfdata::get_survey_samples()] or
#' [gfdata::get_commercial_samples()]
#' @param start_a Starting alpha value for LW estimation. See [stats::nls()].
#' @param start_b Starting beta value for LW estimation. See [stats::nls()].
#' @param maxiter Maximum iterations in estimation. See [stats::nls()].
#' @importFrom stats nls
#' @return The estimated parameters alpha (a) and beta (b) for these data w = a l ^ b
est_lw_params <- function(d,
                          start_a = 5e-2,
                          start_b = 2.0,
                          maxiter = 500){
  stopifnot(nrow(d) > 0)
  stopifnot("weight" %in% names(d))
  stopifnot("length" %in% names(d))
  w <- d$weight
  l <- d$length
  fit <- nls(w ~ a * l ^ b, start = c(a = start_a, b = start_b), control = list(maxiter = maxiter))
  coefficients(fit)
}

#' Fill in missing weights with calculation based on length and input parameters
#' @param d Output from [gfdata::get_commercial_samples()]
#' @param lw_m Output from [est_lw_params] for Males
#' @param lw_f Output from [est_lw_params] for Females
fill_in_weight <- function(d, lw_m, lw_f){
  stopifnot(nrow(d) > 0)
  stopifnot("weight" %in% names(d))
  stopifnot("length" %in% names(d))
  stopifnot("sex" %in% names(d))
  if(all(is.na(d$weight))){
    # All weights are NA so apply global LW relationship to calculate missing weights
    d <- d %>% mutate(weight = ifelse(is.na(weight),
                                      ifelse(sex == 1,
                                             lw_m[1] * length ^ lw_m[2],
                                             lw_f[1] * length ^ lw_f[2]),
                                      weight))
  }else{
    # Estimate a LW relationship for this year/sex only and apply to calculate missing weights
    # If there are too few datapoints, the estimate will fail, and we use the global LW relationship
    loc_lw <- tryCatch(est_lw_params(d),
                       error = function(e){
                         if(unique(d$sex) == 1) lw_m else lw_f
                       })
    d <- d %>%
      mutate(weight = ifelse(is.na(weight), loc_lw[1] * length ^ loc_lw[2], weight))
  }
  d
}

#' Fill in weights using function [fill_in_weight()] for groups made up of `year` and `sex`
#' @param d Output from [gfdata::get_commercial_samples()]
#' @param lw_m Output from [est_lw_params] for Males
#' @param lw_f Output from [est_lw_params] for Females
#' @importFrom dplyr do
add_weights_by_yr_sex_group <- function(d, lw_m, lw_f){
  stopifnot(nrow(d) > 0)
  stopifnot("year" %in% names(d))
  stopifnot("sex" %in% names(d))
  d %>% group_by(year, sex) %>%
    do(fill_in_weight(., lw_m, lw_f)) %>%
    ungroup
}

#' Add `sample_weight` column to the data frame by summing specimen weights
#' @param d Output from [gfdata::get_commercial_samples()]
fill_in_sample_weight <- function(d){
  stopifnot(nrow(d) > 0)
  stopifnot("weight" %in% names(d))
  d %>% mutate(sample_weight = sum(weight))
}

#' Fill in weights using function [fill_in_sample_weight()] for groups made up of
#' `trip_id` and `sample_id`
#' @param d Output from [gfdata::get_commercial_samples()]
add_sample_weight <- function(d){
  stopifnot(nrow(d) > 0)
  stopifnot("trip_id" %in% names(d))
  stopifnot("sample_id" %in% names(d))
  d %>% group_by(trip_id, sample_id) %>%
    do(fill_in_sample_weight(.)) %>%
    ungroup
}

#' Calculate the mean weight and total weight by sex weighted by the sample weight
#' of each sample in each trip in the commercial fishery
#' @param d The data as output from [calc_sample_weights()]
#' @return A data frame of the values described above
calc_mean_total_weight_comm <- function(d){
  stopifnot(nrow(d) > 0)
  stopifnot("year" %in% names(d))
  stopifnot("trip_id" %in% names(d))
  stopifnot("sample_id" %in% names(d))
  stopifnot("sex" %in% names(d))
  stopifnot("weight" %in% names(d))
  stopifnot("sample_weight" %in% names(d))
  stopifnot("catch_weight" %in% names(d))

  q1 <- 1:3
  q2 <- 4:6
  q3 <- 7:9
  q4 <- 10:12
  ret_df <- NULL
  years <- sort(unique(d$year))
  for(yr in seq_along(years)){
    dat <- d %>% filter(year == years[yr])
    trip_ids <- unique(dat$trip_id)
    trip_ids <- trip_ids[!is.na(trip_ids)]
    q1totwtm <- 0
    q2totwtm <- 0
    q3totwtm <- 0
    q4totwtm <- 0
    q1totwtf <- 0
    q2totwtf <- 0
    q3totwtf <- 0
    q4totwtf <- 0
    q1catchwt <- 0
    q2catchwt <- 0
    q3catchwt <- 0
    q4catchwt <- 0
    for(trip_id in seq_along(trip_ids)){
      dat <- d %>% filter(trip_id == trip_ids[!!trip_id])
      sample_ids <- unique(dat$sample_id)
      sample_ids <- sample_ids[!is.na(sample_ids)]
      triptotwtm <- 0
      triptotwtf <- 0
      tripsamplewt <- 0
      tripcatchwt <- 0
      for(sample_id in seq_along(sample_ids)){
        dats <- d %>% filter(sample_id == sample_ids[!!sample_id])
        datsm <- dats %>% filter(sex == 1)
        datsf <- dats %>% filter(sex == 2)
        # Here the mean weight by sex and total weight by sex are weighted by the sample weight
        # Cumulatively add this sample to the current trip EQ 5A and 5B from rocksole appendix
        triptotwtm <-  triptotwtm + sum(datsm$weight) * dats$sample_weight[1]
        triptotwtf <-  triptotwtf + sum(datsf$weight) * dats$sample_weight[1]
        tripsamplewt <- tripsamplewt + dats$sample_weight[1]
        tripcatchwt <- tripcatchwt + dats$catch_weight[1]
        #message("year = ", years[yr], ", trip_id = ", trip_ids[trip_id], ", sample_id = ", sample_ids[sample_id], ", num_weights = ", dats %>% nrow())
      }

      triptotwtm <- triptotwtm / tripsamplewt
      triptotwtf <- triptotwtf / tripsamplewt
      tripmonth <- dat$month[1] # All should be the same for a given trip

      if(tripmonth %in% q1){
        q1catchwt <- q1catchwt + tripcatchwt
        q1totwtm <- q1totwtm + triptotwtm * tripcatchwt
        q1totwtf <- q1totwtf + triptotwtf * tripcatchwt
      }else if(tripmonth %in% q2){
        q2catchwt <- q2catchwt + tripcatchwt
        q2totwtm <- q2totwtm + triptotwtm * tripcatchwt
        q2totwtf <- q2totwtf + triptotwtf * tripcatchwt
      }else if(tripmonth %in% q3){
        q3catchwt <- q3catchwt + tripcatchwt
        q3totwtm <- q3totwtm + triptotwtm * tripcatchwt
        q3totwtf <- q3totwtf + triptotwtf * tripcatchwt
      }else{
        q4catchwt <- q4catchwt + tripcatchwt
        q4totwtm <- q4totwtm + triptotwtm * tripcatchwt
        q4totwtf <- q4totwtf + triptotwtf * tripcatchwt
      }

    }

    q1totwtm <- ifelse(q1catchwt == 0, 0, q1totwtm / q1catchwt)
    q1totwtf <- ifelse(q1catchwt == 0, 0, q1totwtf / q1catchwt)

    q2totwtm <- ifelse(q2catchwt == 0, 0, q2totwtm / q2catchwt)
    q2totwtf <- ifelse(q2catchwt == 0, 0, q2totwtf / q2catchwt)

    q3totwtm <- ifelse(q3catchwt == 0, 0, q3totwtm / q3catchwt)
    q3totwtf <- ifelse(q3catchwt == 0, 0, q3totwtf / q3catchwt)

    q4totwtm <- ifelse(q4catchwt == 0, 0, q4totwtm / q4catchwt)
    q4totwtf <- ifelse(q4catchwt == 0, 0, q4totwtf / q4catchwt)

    divisor <- table(is.na(c(q1totwtm, q2totwtm, q3totwtm, q4totwtm)))["FALSE"]
    yeartotwtm <- sum(q1totwtm, q2totwtm, q3totwtm, q4totwtm, na.rm = TRUE) / divisor
    divisor <- table(is.na(c(q1totwtf, q2totwtf, q3totwtf, q4totwtf)))["FALSE"]
    yeartotwtf <- sum(q1totwtf, q2totwtf, q3totwtf, q4totwtf, na.rm = TRUE) / divisor
    ret_df <- rbind(ret_df, c(years[yr], yeartotwtm, yeartotwtf))
  }
  ret_df
}

#' Calculate the mean weight and total weight by sex weighted by the sample weight
#' of each sample in each trip in a survey
#' @param d The data as output from [calc_sample_weights()]
#' @return A data frame of the values described above
calc_mean_total_weight_surv <- function(d){
  stopifnot(nrow(d) > 0)
  stopifnot("year" %in% names(d))
  stopifnot("trip_id" %in% names(d))
  stopifnot("sample_id" %in% names(d))
  stopifnot("sex" %in% names(d))
  stopifnot("sample_weight" %in% names(d))
  stopifnot("catch_weight" %in% names(d))

  retdf <- NULL
  years <- sort(unique(d$year))
  for(yr in seq_along(years)){
    dat <- d %>% filter(year == years[yr])
    trip_ids <- unique(dat$trip_id)
    trip_ids <- trip_ids[!is.na(trip_ids)]
    yrcatchwt <- 0
    for(trip_id in seq_along(trip_ids)){
      dat <- d %>% filter(trip_id == trip_ids[!!trip_id])
      sample_ids <- unique(dat$sample_id)
      sample_ids <- sample_ids[!is.na(sample_ids)]
      triptotwtm <- 0
      triptotwtf <- 0
      tripsamplewt <- 0
      tripcatchwt <- 0
      for(sample_id in seq_along(sample_ids)){
        dats <- d %>% filter(sample_id == sample_ids[!!sample_id])
        datsm <- dats %>% filter(sex == 1)
        datsf <- dats %>% filter(sex == 2)

        # Here the total weight by sex is weighted by the sample weight
        # Cumulatively add this sample to the current trip EQ 5A and 5B from rocksole appendix
        triptotwtm <-  triptotwtm + sum(datsm$weight) * dats$sample_weight[1]
        triptotwtf <-  triptotwtf + sum(datsf$weight) * dats$sample_weight[1]
        tripsamplewt <- tripsamplewt + dats$sample_weight[1]
      }
      triptotwtm <- triptotwtm / tripsamplewt / dat$catch_weight[1]
      triptotwtf <- triptotwtf / tripsamplewt / dat$catch_weight[1]
      yrcatchwt <- yrcatchwt + dat$catch_weight[1]
    }
    triptotwtm <- triptotwtm * yrcatchwt
    triptotwtf <- triptotwtf * yrcatchwt
    retdf <- rbind(retdf, c(years[yr], triptotwtm, triptotwtf))
  }
  retdf
}

#' Apply the LW relationship to the vector of `lengths`
#' @param lengths A vector of lengths
#' @param lw_params A vector of two values, alpha and Beta for the LW relationship
#' @return A vector of weights
apply_lw_rel <- function(lengths, lw_params){
  lw_params[1] * lengths ^ lw_params[2]
}

#' Calculate the sample weights given the data have many individual weights
#' If sample weights are not recorded, they will be filled in
#' @param d The data frame as output by [convert_length_to_weight_by_sex()]
#' @param lw_params A vector of two values, alpha and Beta for the LW relationship
#' @return A data frame with the sample weights filled in
calc_sample_weights <- function(d, lw_params){
  stopifnot(nrow(d) > 0)
  stopifnot("sample_id" %in% names(d))
  stopifnot("weight" %in% names(d))
  stopifnot("sample_weight" %in% names(d))

  ret_df <- NULL
  sample_ids <- unique(d$sample_id)
  for(sid in seq_along(sample_ids)){
    dat <- d %>% filter(sample_id == sample_ids[sid])
    if(any(is.na(dat$sample_weight))){
      dat$sample_weight <- sum(dat$weight)
    }
    ret_df <- rbind(ret_df, dat)
  }
  ret_df
}

#' Convert all lengths to weights for each sex for the input data frame `dat`
#' @param d A data frame returned by running [gfdata::get_survey_samples()] or
#' [gfdata::get_commercial_samples()]
#' @param lw_params A two-element list of vectors of alpha, Beta with males first and females second
#' in the list
#' @return A data frame with weights calculated from the LW relationship
convert_length_to_weight_by_sex <- function(d, lw_params){
  stopifnot(nrow(d) > 0)
  stopifnot("weight" %in% names(d))
  stopifnot("length" %in% names(d))
  stopifnot("sex" %in% names(d))

  # Save the records which have recorded weights. They will be appended before returning.
  dw <- d %>% filter(!is.na(weight))

  # Only for records in which there is no weight but there is a length
  d <- d %>% filter(!is.na(length), is.na(weight))

  dm <- d %>% filter(sex == 1)
  dm$weight <- apply_lw_rel(dm$length, lw_params[[1]])
  df <- d %>% filter(sex == 2)
  df$weight <- apply_lw_rel(df$length, lw_params[[2]])
  rbind(dw, dm, df)
}

#' Calculate the proportion of females by year for the commercial fishery
#' @description
#' Generate weights for all records in `dat` that have lengths but no weights associated
#' Pseudocode:
#' For each year:
#'   For each trip:
#'     For each sample ID:
#'       Calculate sex-specific mean weights for this sample
#'       Calculate sex-specific catch weight for this sample
#'     EndFor
#'     Calculate sex-specific mean weight for this trip
#'     Calculate sex-specific total catch weight for this trip
#'   EndFor
#'   Calculate the sex-specific mean weights for each quarter (3-months)
#'   Calculate the sex-specific total catch weights for each quarter (3-months) weighted by total catch weight of sampled trips in the quarter
#' EndFor
#' @param dat A data frame returned by running [gfdata::get_survey_samples()] or
#' [gfdata::get_commercial_samples()]
#' @param lw_params A two-element list of vectors of alpha, Beta with males first and females second
#' in the list
#' @param data_source_name Name to place in the data frame to describe these data
#' @param ... Absorb arguments meant for other functions
#' @return A data frame of totals of males and females and proportions of females
calc_sex_props_comm <- function(dat,
                                lw_params = NA,
                                data_source_name = "default",
                                ...){

  # 1. Convert all lengths to weights by sex where then don't exist
  d <- convert_length_to_weight_by_sex(dat, lw_params)

  # 2. Calculate the sample weights given the lengths and LW parameters
  #    Convert lengths to weights for all records (by sex)
  d <- calc_sample_weights(d, lw_params)

  # 3. Calculate the mean weight and total weight by sex weighted by the sample weight
  #    of each sample in each trip
  d <- calc_mean_total_weight_comm(d) %>%
    as_tibble(.name_repair = function(names) c("year", "total_male", "total_female"))

  # 4. Calculate proportions female
  d %>%
    mutate(prop_female = total_female / (total_male + total_female),
           data_source = data_source_name) %>%
    select(-c("total_male", "total_female"))
}

#' Calculate the proportion of females by year for the surveys
#' @param dat A data frame returned by running [gfdata::get_survey_samples()] or
#' [gfdata::get_commercial_samples()]
#' @param lw_params A two-element list of vectors of alpha, Beta with males first and females second
#' in the list
#' @param surv_series Value of the `survey_series_id` in the data frame to filter
#' @param ... Absorb arguments meant for other functions
#' @return A list of data frames of totals of males and females and proportions of females for each survey
calc_sex_props_surv <- function(dat,
                                lw_params = NA,
                                surv_series = 1,
                                ...){

  # 1. Convert all lengths to weights by sex where then don't exist
  d <- convert_length_to_weight_by_sex(dat, lw_params)

  # 2. Calculate the sample weights given the lengths and LW parameters
  #    Convert lengths to weights for all records (by sex)
  d <- calc_sample_weights(d, lw_params)

  #3. Calculate the mean weight and total weight by sex weighted by the sample weight
  #    of each sample in each trip, for each survey
  d <- d %>% filter(survey_series_id == surv_series)

  d_surv <- calc_mean_total_weight_surv(d) %>%
    as_tibble(.name_repair = function(names) c("year", "total_male", "total_female"))

  # 4. Calculate proportions female
  d_surv %>%
    mutate(prop_female = total_female / (total_male + total_female)) %>%
    select(-c("total_male", "total_female"))
}

#' Create a matrix of proportion female by year from the surveys given in the vector
#' @param dat A data frame returned by running [gfdata::get_survey_samples()] or
#' [gfdata::get_commercial_samples()]
#' @param type One of "commercial" or "survey"
#' @param included_vessels A vector of GFBIO vessel IDs to include in the output. They are
#'  filtered here as opposed to earlier because for example freezer trawlers have no weight samples,
#'  so the wet boat weights are needed to determine parameters for LW relationship which can then
#'  be applied to freezer trawler length samples.
#' @param ... Arguments passed to [calc_sex_props_comm()] and [calc_sex_props_surv()]
#' @importFrom lubridate month
#' @return A data frame of totals of males and females and proportions of females
make_sex_props <- function(dat,
                           type = "commercial",
                           included_vessels = NA,
                           ...){

  datm <- dat %>% filter(sex == 1)
  datf <- dat %>% filter(sex == 2)

  lw_params <- list(est_lw_params(datm), est_lw_params(datf))
  #cat("Estimated LW parameters for all data: Males alpha = ", lw_params[[1]][1], ", beta = ", lw_params[[1]][2], "\n")
  #cat("Estimated LW parameters for all data: Females alpha = ", lw_params[[2]][1], ", beta = ", lw_params[[2]][2], "\n")

  # Filter vessels out after calculation of global LW parameters. These would be freezer trawlers usually
  # but could be any set of vessels.
  if(!is.na(included_vessels[1]) && "vessel_id" %in% names(dat)){
    dat <- dat %>% filter(vessel_id %in% included_vessels)
  }

  # Add month column to data
  dat <- dat %>%
    mutate(month = month(trip_start_date))
  if(type == "commercial"){
    p <- calc_sex_props_comm(dat,
                             lw_params = lw_params,
                             ...)
  }else{
    p <- calc_sex_props_surv(dat,
                             lw_params = lw_params,
                             ...)
  }
  p
}

#' Calculate the proportions female for the commercial data
#'
#' @param d Output from [gfdata::get_commercial_samples()]
#' @param areas Vector of area numbers as character strings, eg. "03" is area 3C
#' @param start_year Years equal or greater than this and less or equal to `end_year` will be returned
#' @param end_year Years equal or less than this and greater or equal to `start_year` will be returned
#' @param species_category Values of `species_category_code` to include. Use NA to include all included in data:
#' SPECIES_CATEGORY_CODE        SPECIES_CATEGORY_DESC
#' 0                                          UNKNOWN
#' 1                                         UNSORTED
#' 2                          SORTED (UNK. CRITERION)
#' 3                                          KEEPERS
#' 4                                        DISCARDED
#' 5                                          REMAINS
#' 6                        LONGLINE - FISH HEAD ONLY
#' 8 LONGLINE/JIG - FISH LOST AT RAIL/LOST AT SURFACE
#'@param sample_type Values of `sample_type_code` to include. Use NA to include all included in data:
#' SAMPLE_TYPE_CODE    SAMPLE_TYPE_DESC ROW_VERSION
#'  0                                       UNKNOWN
#'  1                                  TOTAL  CATCH
#'  2                                        RANDOM
#'  4                                      SELECTED
#'  5                                   STRATIFIED
#'  6             RANDOM FROM RANDOMLY ASSIGNED SET
#'  7   RANDOM FROM SET AFTER RANDOMLY ASSIGNED SET
#'  8    RANDOM FROM SET REQUESTED BY VESSEL MASTER
#'  9                            SELECTED JUVENILES
#' 10                               SELECTED ADULTS
#' 11               OTHER - SEE SAMPLE_TYPE_COMMENT
#' 12                    SELECTED FIRST N SPECIMENS
#' 13  SELECTED FIRST N SPECIMENS - WHOLE FISH ONLY
#' 14                       SELECTED TAG RECOVERIES
#' 15                                SELECTED MALES
#' 16                              SELECTED FEMALES
#' 17 UNKNOWN SAMPLE TYPE FOR NMFS TRIENNIAL SURVEY
#' @param gear Values of `gear_code` to include. Use NA to include all included in data:
#'  GEAR_CODE                                GEAR_DESC
#'  0                                          UNKNOWN
#'  1                                     BOTTOM TRAWL
#'  2                                             TRAP
#'  3                                          GILLNET
#'  4                                         HANDLINE
#'  5                                         LONGLINE
#'  6                                   MIDWATER TRAWL
#'  7                                            TROLL
#'  8                                    UNKNOWN TRAWL
#'  9                                            SEINE
#' 10                                              JIG
#' 11                                     SHRIMP TRAWL
#' 12                          RECREATIONAL ROD & REEL
#' 13                                       PRAWN TRAP
#' 14                                     TUCKER TRAWL
#' 15 MULTI-NET LARGE PLANKTON SAMPLER: HYDROBIOS/MINI
#' 16                                  TWIN BEAM TRAWL
#' 17                                  HAND SEINE 1/2"
#' 18                                PURSE SEINE 3/16"
#' 19                                        BONGO NET
#' 20                                    PROFILING CTD
#' 21                                           CAMERA
#' 22        DROP ACCOUSTIC INFORMATION SYSTEM (DAISY)
#' 23                                  ROSETTE SAMPLER
#' 24                                       ARGO FLOAT
#' 25                                          MOCNESS
#' @param data_source_name Name to place in the `data_source` column for all records in return data frame
#' @param ... Arguments not intended for this function
#' @export
props_comm <- function(d,
                       areas = c("03", "04", "05", "06", "07", "08", "09"),
                       start_year = 1996,
                       end_year = 2020,
                       species_category = c(0, 1, 2, 3, 4),
                       sample_type = c(1, 2, 6, 7, 8),
                       gear = c(1, 8),
                       data_source_name = "commercial_coastwide",
                       ...){

  d <- d %>%
    filter(year >= start_year,
           year <= end_year,
           major_stat_area_code %in% areas,
           sex %in% 1:2,
           !is.na(length))

  if(!all(is.na(species_category))){
    d <- d %>% filter(species_category_code %in% species_category)
  }
  if(!all(is.na(sample_type))){
    d <- d %>% filter(sample_type_code %in% sample_type)
  }
  if(!all(is.na(gear))){
    d <- d %>% filter(gear_code %in% gear)
  }
  d <- d %>%
    select(trip_start_date, major_stat_area_code, trip_id, sample_id, year, sex, length, weight, catch_weight, vessel_id)

  if(all(is.na(d$weight))){
    stop("All weights are NA for your dataset, LW relationship cannot be calculated.",
         call. = FALSE)
  }
  global_lw_m <- est_lw_params(d %>% filter(sex == 1))
  global_lw_f <- est_lw_params(d %>% filter(sex == 2))

  d <- add_weights_by_yr_sex_group(d, global_lw_m, global_lw_f)
  d <- add_sample_weight(d)

  make_sex_props(dat = d,
                 type = "commercial",
                 data_source_name = data_source_name,
                 ...)
}

#' Calculate the proportions female for the survey data
#' @param surv_series A vector of integers representing the survey ids to use
#' @param surv_series_names A vector of names of surveys to use for the given `surv_series` values
#' @param surv_sets A data frame object returned from [gfdata::get_survey_sets()]
#' @param surv_samples A data frame object returned from [gfdata::get_survey_samples()]
#' @param ... Arguments not intended for this function
#' @importFrom dplyr right_join distinct tally desc rename
#' @importFrom furrr future_map
#' @importFrom purrr map2 map_df
#' @return A data frame containing proportions of females for the required surveys
#' @export
props_surv <- function(surv_series = 1:4,
                       surv_series_names = c("qcsss", "hsmas", "hsss", "wcviss"),
                       surv_sets = survey_sets,
                       surv_samples = survey_samples,
                       ...){

  surv_catch <- surv_sets %>% select(fishing_event_id, catch_weight)
  surv_samples <- surv_samples %>% right_join(surv_catch, by = "fishing_event_id")
  surv_samples <- surv_samples %>%
    group_by(sample_id) %>%
    mutate(sample_weight = sum(weight, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(trip_id = year)

  future_map(surv_series, ~{
    make_sex_props(dat = surv_samples,
                   surv_series = .x,
                   type = "survey",
                   ...)
  }) %>%
    map2(surv_series_names, ~{
      .x %>% mutate(data_source = .y)
    }) %>%
    map_df(~{.x})

  # Table listing of surveys
  #survey_samples %>% select(survey_series_id, survey_abbrev, survey_series_desc) %>% distinct()
}

#' Calculate all proportions female (commercial and all surveys)
#'
#' @param comm_samples Output from [gfdata::get_commercial_samples()]
#' @param ... Arguments to pass to [props_comm()] and [props_surv()]
#' @importFrom dplyr bind_rows
#' @return A data frame containing all female proportions
#' @export
props_all <- function(comm_samples, ...){

  comm <- props_comm(comm_samples,
                     areas = c("03", "04", "05", "06", "07", "08", "09"),
                     data_source_name = "Coastwide",
                     ...)

  comm_3cd <- props_comm(comm_samples,
                         areas = c("03", "04"),
                         data_source_name = "3CD",
                         ...)

  comm_5abcde <- props_comm(comm_samples,
                            areas = c("05", "06", "07", "08", "09"),
                            data_source_name = "5ABCDE",
                            ...)

  surv <- props_surv(...)

  comm %>%
    bind_rows(comm_3cd, comm_5abcde, surv)
}

#' Summarize the data used to make the proportions female table
#'
#' @param comm_samples Commercial samples as extracted using [gfdata::get_commercial_samples()]
#' @rdname props_comm
#' @importFrom tidyr pivot_wider
#' @return A data frame summarizing the data
#' @export
props_comm_data_summary <- function(comm_samples,
                                    areas = c("03", "04", "05", "06", "07", "08", "09"),
                                    start_year = 1996,
                                    end_year = 2019,
                                    species_category = c(0, 1, 2, 3, 4),
                                    sample_type = c(1, 2, 6, 7, 8),
                                    gear = c(1, 8)){

  d <- comm_samples %>%
    filter(year >= start_year,
           year <= end_year,
           major_stat_area_code %in% areas,
           sex %in% 1:2,
           !is.na(length))
  if(!all(is.na(species_category))){
    d <- d %>% filter(species_category_code %in% species_category)
  }
  if(!all(is.na(sample_type))){
    d <- d %>% filter(sample_type_code %in% sample_type)
  }
  if(!all(is.na(gear))){
    d <- d %>% filter(gear_code %in% gear)
  }
  d <- d %>%
    select(trip_start_date, major_stat_area_code, trip_id, sample_id, year, sex, length, weight, catch_weight)

  #if(all(is.na(d$weight))){
  #  stop("All weights are NA for your dataset, LW relationship cannot be calculated.",
  #       call. = FALSE)
  #}

  num_trips <- d %>%
    group_by(year) %>%
    select(year, trip_id) %>%
    distinct %>%
    tally(name = "Number of trips") %>%
    ungroup %>%
    arrange(year, desc(`Number of trips`))

  num_samples <- d %>%
    group_by(year) %>%
    select(year, sample_id) %>%
    distinct %>%
    tally(name = "Number of samples") %>%
    ungroup %>%
    arrange(year, desc(`Number of samples`))

  num_weights <- d %>%
    group_by(year, sample_id, sex) %>%
    summarize(year, sex, num_weights = n()) %>%
    distinct %>%
    ungroup %>%
    group_by(year, sex) %>%
    select(year, sex, num_weights) %>%
    summarize(year, sex, `Number of weights` = sum(num_weights)) %>%
    distinct %>%
    ungroup %>%
    arrange(year, desc(`Number of weights`))

  num_trips %>%
    left_join(num_samples, by = "year") %>%
    left_join(num_weights, by = "year") %>%
    rename(Year = year, Sex = sex) %>%
    arrange(Year, Sex) %>%
    select(Year, Sex, everything()) %>%
    mutate(Sex = ifelse(Sex == 1, "Number of weights - Male", "Number of weights - Female")) %>%
    pivot_wider(names_from = "Sex", values_from = "Number of weights")

}

#' Summarize the data used to make the proportions female table
#'
#' @rdname props_surv
#'
#' @param surv_samples Output from [gfdata::get_survey_samples()]
#' @param surv_series Values of `survey_series_id` which is a column of `surv_samples`
#' @param surv_series_names Names to be associated to the values of `surv_series`
#' @return A data frame summarizing the data
#' @export
props_surv_data_summary <- function(surv_samples,
                                    surv_series = 1:4,
                                    surv_series_names = c("QCS Synoptic",
                                                          "HS Multispecies",
                                                          "HS Synoptic",
                                                          "WCVI Synoptic")){

  d <- surv_samples %>%
    group_by(sample_id) %>%
    mutate(sample_weight = sum(weight, na.rm = TRUE)) %>%
    filter(survey_series_id %in% surv_series,
           sex %in% 1:2) %>%
    ungroup() %>%
    mutate(trip_id = year)

  num_samples <- d %>%
    group_by(survey_series_id, year, sample_id) %>%
    select(survey_series_id, year, sample_id) %>%
    distinct %>%
    ungroup %>%
    group_by(survey_series_id, year) %>%
    tally(name = "Number of samples") %>%
    ungroup %>%
    mutate(survey_name = surv_series_names[survey_series_id]) %>%
    select(-survey_series_id) %>%
    rename(Survey = survey_name,
           Year = year) %>%
    select(Survey, Year, everything()) %>%
    arrange(Survey, Year, `Number of samples`)

  num_weights <- d %>%
    group_by(survey_series_id, year, sample_id, sex) %>%
    summarize(survey_series_id, year, sex, num_weights = n()) %>%
    distinct %>%
    ungroup %>%
    group_by(survey_series_id, year, sex) %>%
    select(survey_series_id, year, sex, num_weights) %>%
    summarize(survey_series_id, year, sex, `Number of weights` = sum(num_weights)) %>%
    distinct %>%
    ungroup %>%
    mutate(Survey = surv_series_names[survey_series_id]) %>%
    rename(Year = year,
           Sex = sex) %>%
    select(-survey_series_id) %>%
    select(Survey, Year, everything()) %>%
    arrange(Survey, Year, `Number of weights`)

  num_samples %>% left_join(num_weights, by = c("Survey", "Year")) %>%
    mutate(Sex = ifelse(Sex == 1, "Number of weights - Male", "Number of weights - Female")) %>%
    pivot_wider(names_from = "Sex", values_from = "Number of weights")
}