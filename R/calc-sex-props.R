est_lw_params <- function(dat){
  # Estimate the length/weight parameters for the dataset 'dat'
  # Returns the estimated parameters alpha (a) and beta (b) for these data (w=al^b).)
  # If there are no data in the dat table, the defaultLW will be returned

  # Filter data for individuals with both length and weight
  dat <- dat %>% filter(!is.na(length),
                        !is.na(weight))

  cat("Estimating LW parameters. Samples with both length and weight data: \n")

  if(nrow(dat) > 0){
    tmp <- aggregate(weight ~ year + sample_id, data = dat, FUN = "length")
    names(tmp)[3] <- "num_records"
    w <- dat$weight
    l <- dat$length
    fit <- nls(w ~ a * l ^ b, start = c(a = 0.5, b = 2.0), control = list(maxiter = 500))
    print(tmp)
    cat("Total number of records used in LW estimation: ", sum(tmp$num_records), "\n\n")
      return(coefficients(fit))
  }
  cat("est_lw_params: No records, returning NA.\n\n")
  NA
}

calc_mean_total_weight_comm <- function(d, weight_adj = 1){
  # Calculate the mean weight and total weight by sex weighted by the sample weight
  # of each sample in each trip

  # Quarters, the months included
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
      dat <- dat %>% filter(trip_id == trip_ids[!!trip_id])
      sample_ids <- unique(dat$sample_id)
      sample_ids <- sample_ids[!is.na(sample_ids)]
      triptotwtm <- 0
      triptotwtf <- 0
      tripsamplewt <- 0
      tripcatchwt <- 0
      for(sample_id in seq_along(sample_ids)){
        dats <- dat %>% filter(sample_id == sample_ids[!!sample_id])
        datsm <- dats %>% filter(sex == 1)
        datsf <- dats %>% filter(sex == 2)

        # Here the mean weight by sex and total weight by sex are weighted by the sample weight
        # Cumulatively add this sample to the current trip EQ 5A and 5B from rocksole appendix
        triptotwtm <-  triptotwtm + sum(datsm$weight) / weight_adj * dats$sample_weight[1]
        triptotwtf <-  triptotwtf + sum(datsf$weight) / weight_adj * dats$sample_weight[1]
        tripsamplewt <- tripsamplewt + dats$sample_weight[1]
        tripcatchwt <- tripcatchwt + dats$catch_weight[1]
        #if(is.na(tripcatchwt)) browser()

      }
      triptotwtm <- triptotwtm / tripsamplewt
      triptotwtf <- triptotwtf / tripsamplewt
      tripmonth <- dat$month[1] # All should be the same for a given trip
      if(tripmonth %in% q1){
        q1catchwt <- q1catchwt + tripcatchwt
        q1totwtm <- q1totwtm + triptotwtm * tripcatchwt
        q1totwtf <- q1totwtf + triptotwtf * tripcatchwt
      }
      if(tripmonth %in% q2){
        q2catchwt <- q2catchwt + tripcatchwt
        q2totwtm <- q2totwtm + triptotwtm * tripcatchwt
        q2totwtf <- q2totwtf + triptotwtf * tripcatchwt
      }
      if(tripmonth %in% q3){
        q3catchwt <- q3catchwt + tripcatchwt
        q3totwtm <- q3totwtm + triptotwtm * tripcatchwt
        q3totwtf <- q3totwtf + triptotwtf * tripcatchwt
      }
      if(tripmonth %in% q4){
        q4catchwt <- q4catchwt + tripcatchwt
        q4totwtm <- q4totwtm + triptotwtm * tripcatchwt
        q4totwtf <- q4totwtf + triptotwtf * tripcatchwt
      }
    }

    q1totwtm <- q1totwtm / q1catchwt
    q1totwtf <- q1totwtf / q1catchwt

    q2totwtm <- q2totwtm / q2catchwt
    q2totwtf <- q2totwtf / q2catchwt

    q3totwtm <- q3totwtm / q3catchwt
    q3totwtf <- q3totwtf / q3catchwt

    q4totwtm <- q4totwtm / q4catchwt
    q4totwtf <- q4totwtf / q4catchwt

    divisor <- table(is.na(c(q1totwtm, q2totwtm, q3totwtm, q4totwtm)))["FALSE"]
    yeartotwtm <- sum(q1totwtm, q2totwtm, q3totwtm, q4totwtm, na.rm=TRUE) / divisor
    divisor <- table(is.na(c(q1totwtf, q2totwtf, q3totwtf, q4totwtf)))["FALSE"]
    yeartotwtf <- sum(q1totwtf, q2totwtf, q3totwtf, q4totwtf, na.rm=TRUE) / divisor
    ret_df <- rbind(ret_df, c(years[yr], yeartotwtm, yeartotwtf))
  }
  ret_df
}

calc_mean_total_weight_surv <- function(d, weight_adj = 1){
  # Calculate the mean weight and total weight by sex weighted by the sample weight
  # of each sample in each trip

  retdf <- NULL
  years <- sort(unique(d$year))
  for(yr in seq_along(years)){
    dat <- d %>% filter(year == years[yr])
    trip_ids <- unique(dat$trip_id)
    trip_ids <- trip_ids[!is.na(trip_ids)]
    yrcatchwt <- 0
    for(trip_id in seq_along(trip_ids)){
      dat <- dat %>% filter(trip_id == trip_ids[!!trip_id])
      sample_ids <- unique(dat$sample_id)
      sample_ids <- sample_ids[!is.na(sample_ids)]
      triptotwtm <- 0
      triptotwtf <- 0
      tripsamplewt <- 0
      tripcatchwt <- 0
      for(sample_id in seq_along(sample_ids)){
        dats <- dat %>% filter(sample_id == sample_ids[!!sample_id])
        datsm <- dats %>% filter(sex == 1)
        datsf <- dats %>% filter(sex == 2)

        # Here the total weight by sex is weighted by the sample weight
        # Cumulatively add this sample to the current trip EQ 5A and 5B from rocksole appendix
        triptotwtm <-  triptotwtm + sum(datsm$weight) / weight_adj * dats$sample_weight[1]
        triptotwtf <-  triptotwtf + sum(datsf$weight) / weight_adj * dats$sample_weight[1]
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

apply_lw_rel <- function(lengths, lw_params){
  # Apply the LW relationship to the vector of lengths
  # return the vector of weights
  lw_params[1] * lengths ^ lw_params[2]
}

calc_sample_weights <- function(d, lw_params, weight_adj = 1){
  # Calculate the sample weights given the data have many individual weights
  # If sample weights are not recorded, they will be filled in
  ret_df <- NULL # return dataframe

  sample_ids <- unique(d$sample_id)
  for(sid in 1:length(sample_ids)){
    dat <- d %>% filter(sample_id == sample_ids[sid])
    if(any(is.na(dat$sample_weight))){
      dat$sample_weight <- sum(dat$weight) / weight_adj
    }
    ret_df <- rbind(ret_df, dat)
  }
  return(ret_df)
}

convert_length_to_weight_by_sex <- function(dat, lw_params){
  # Convert all lengths to weights for each sex for the input data frame dat
  # lw_params is a two-element list with males first and females second
  #  each list element is a vector of two parameters, alpha and beta
  # to be used in the LW relationship

  # Only for records in which there is no weight but there is a length
  d <- dat %>% filter(!is.na(length), is.na(weight))

  # Save the records which have recorded weights. They will be appended before returning.
  dw <- dat %>% filter(!is.na(weight))

  dm <- d %>% filter(sex == 1)
  dm$weight <- apply_lw_rel(dm$length, lw_params[[1]])
  df <- d %>% filter(sex == 2)
  df$weight <- apply_lw_rel(df$length, lw_params[[2]])
  rbind(dw, dm, df)
}

calc_sex_props_comm <- function(dat,
                                lw_params = NA,
                                areas = 3:9,
                                weight_adj = 1){
  # Calculate the proportion of females by year.
  # lw_params is a list of 2 vectors, male then female parameters for LW relationship
  # Generate weights for all records in dat that have lengths but no weights associated
  # Pseudocode:
  # For each year:
  #   For each trip:
  #     For each sample ID:
  #       Calculate sex-specific mean weights for this sample
  #       Calculate sex-specific catch weight for this sample
  #     EndFor
  #     Calculate sex-specific mean weight for this trip
  #     Calculate sex-specific total catch weight for this trip
  #   EndFor
  #   Calculate the sex-specific mean weights for each quarter (3-months)
  #   Calculate the sex-specific total catch weights for each quarter (3-months) weighted by total catch weight of sampled trips in the quarter
  # EndFor

  # 1. Convert all lengths to weights by sex where then don't exist
  d <- convert_length_to_weight_by_sex(dat, lw_params)

  # 2. Calculate the sample weights given the lengths and LW parameters
  #    Convert lengths to weights for all records (by sex)
  d <- calc_sample_weights(d, lw_params, weight_adj)

  # 3. Calculate the mean weight and total weight by sex weighted by the sample weight
  #    of each sample in each trip
  d <- calc_mean_total_weight_comm(d, weight_adj)

  # 4. Calculate proportions female
  propfemale <- d[,3] / (d[,2] + d[,3])
  d <- cbind(d, propfemale)
  colnames(d) <- c("Year", "totwtMale", "totwtFemale", "propFemale")
  d
}

calc_sex_props_surv <- function(dat,
                                lw_params = NA,
                                weight_adj = 1){
  # Calculate the proportion of females by year.
  # lw_params is a list of 2 vectors, male then female parameters for LW relationship
  # Generate weights for all records in dat that have lengths but no weights associated

  # 1. Convert all lengths to weights by sex where then don't exist
  d <- convert_length_to_weight_by_sex(dat, lw_params)

  # 2. Calculate the sample weights given the lengths and LW parameters
  #    Convert lengths to weights for all records (by sex)
  d <- calc_sample_weights(d, lw_params, weight_adj)

  # 3. Calculate the mean weight and total weight by sex weighted by the sample weight
  #    of each sample in each trip, for each survey
  qcsss   <- d %>% filter(survey_series_id == 1)
  hsmas   <- d %>% filter(survey_series_id == 2)
  hsss    <- d %>% filter(survey_series_id == 3)
  wcviss  <- d %>% filter(survey_series_id == 4)

  dqcsss  <- calc_mean_total_weight_surv(qcsss, weight_adj)
  dhsmas  <- calc_mean_total_weight_surv(hsmas, weight_adj)
  dhsss   <- calc_mean_total_weight_surv(hsss, weight_adj)
  dwcviss <- calc_mean_total_weight_surv(wcviss, weight_adj)

  # 4. Calculate proportions female
  prop <- dqcsss[,3] / (dqcsss[,2] + dqcsss[,3])
  propstr <- sprintf("%0.3f", prop)
  dqcsss <- cbind(dqcsss, propstr)
  colnames(dqcsss) <- c("Year","totwtMale","totwtFemale","propFemale")

  prop <- dhsmas[,3] / (dhsmas[,2] + dhsmas[,3])
  propstr <- sprintf("%0.3f", prop)
  dhsmas <- cbind(dhsmas, propstr)
  colnames(dhsmas) <- c("Year","totwtMale","totwtFemale","propFemale")

  prop <- dhsss[,3] / (dhsss[,2] + dhsss[,3])
  propstr <- sprintf("%0.3f", prop)
  dhsss <- cbind(dhsss, propstr)
  colnames(dhsss) <- c("Year","totwtMale","totwtFemale","propFemale")

  prop <- dwcviss[,3] / (dwcviss[,2] + dwcviss[,3])
  propstr <- sprintf("%0.3f", prop)
  dwcviss <- cbind(dwcviss, propstr)
  colnames(dwcviss) <- c("Year","totwtMale","totwtFemale","propFemale")
  list(qcsss = dqcsss, hsmas = dhsmas, hsss = dhsss, wcviss = dwcviss)
}

make_sex_props <- function(dat,
                           ssid = NA,
                           areas = 3:9,
                           byweight = TRUE,
                           weight_adj = 1,
                           type = "commercial"){
  # Create a matrix of proportion female by year from the surveys given in the vector
  dat <- dat %>%
    filter(year >= 1996)

  if(type == "commercial"){
    dat <- dat %>%
      filter(major_stat_area_code %in% c("03", "04", "05", "06", "07", "08", "09"))
  }

  datm <- dat %>% filter(sex == 1)
  datf <- dat %>% filter(sex == 2)

  lw_params <- list(est_lw_params(datm), est_lw_params(datf))
  cat("Estimated LW parameters for all data: Males alpha = ", lw_params[[1]][1], ", beta = ", lw_params[[1]][2], "\n")
  cat("Estimated LW parameters for all data: Females alpha = ", lw_params[[2]][1], ", beta = ", lw_params[[2]][2], "\n")

  # Add month column to data
  dat <- dat %>%
    mutate(month = lubridate::month(trip_start_date))
  if(type == "commercial"){
    p <- calc_sex_props_comm(dat,
                             lw_params = lw_params,
                             areas = areas,
                             weight_adj = weight_adj)
  }else{
    p <- calc_sex_props_surv(dat,
                             lw_params = lw_params,
                             weight_adj = weight_adj)
  }
  p
}

props_fishery <- function(){
  make_sex_props(dat = commercial_samples,
                 ssid = ssid,
                 areas = 3:9,
                 byweight = TRUE,
                 weight_adj = 1000,
                 type = "commercial")

  # Table listing of surveys
  #survey_samples %>% select(survey_series_id, survey_abbrev, survey_series_desc) %>% distinct()
}

props_survey <- function(surv_series = 1:4,
                         surv_sets = survey_sets,
                         surv_samples = survey_samples){

  surv_catch <- surv_sets %>% select(fishing_event_id, catch_weight)
  surv_samples <- surv_samples %>% right_join(surv_catch, by = "fishing_event_id")
  surv_samples <- surv_samples %>%
    group_by(sample_id) %>%
    mutate(sample_weight = sum(weight, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(trip_id = year)

  make_sex_props(dat = surv_samples, weight_adj = 1000, type = "survey")

  # Table listing of surveys
  #survey_samples %>% select(survey_series_id, survey_abbrev, survey_series_desc) %>% distinct()
}


