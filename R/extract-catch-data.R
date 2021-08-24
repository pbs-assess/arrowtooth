#' Extract the female catch data for pasting into the iSCAM data file. Be careful, the default filtering values
#' in [props_comm()] which are passed through `...` have default value which will filter your data
#' if you do not provide your own values.
#'
#' @details Proportions female are calculated and multiplied by the total catch, which is a sum of all
#' catch sources (see group_by/summarize clause in ct assignment below).
#' For years with no samples (missing years), the mean of all other years' proportions are used.
#' The year range is determined by what is set in [props_comm()], which is passed through `...`
#'
#' @param catch Data frame as output by [gfdata::get_catch()]
#' @param comm_samples Data frame as output by [gfdata::get_commercial_samples()]
#' @param gear_num Number of gear to be written in output file
#' @param female_only If `TRUE`, indices and wts will be multiplied by the proportion female
#' @param append If `TRUE`, append the output to the file. If `FALSE`, overwrite the file
#' @param write_to_file If `TRUE`, write the output to the file. If `FALSE`, return the data frame
#' @param extrap_years The number of years to look into the past to extrapolate the last year's catch based
#' on the last day of catch in the last year. If `NA`, do not do an extrapolation of the last year's catch
#' @param ... Arguments to be passed on to [props_comm()] and [gfplot::tidy_catch()]
#'
#' @return A data frame you can cut and paste into the iSCAM data file
#' @export
#' @importFrom gfplot tidy_catch
#' @importFrom lubridate as_date day month year yday ymd
#' @examples
#' \dontrun{
#' All for Feb 21 - Feb 20 fishing year:
#' extract_catch_data(catch, commercial_samples, gear_num = 1, append = FALSE,
#' species_category = 1, month_fishing_starts = 2, day_fishing_starts = 21, female_only = FALSE)
#'
#' Freezer trawlers:
#' extract_catch_data(extract_fleet_catch(catch, include = TRUE),
#'   commercial_samples, gear_num = 1, append = FALSE, species_category = 1,
#'   month_fishing_starts = 1, day_fishing_starts = 1, female_only = FALSE)
#'
#' Non-Freezer trawlers:
#' extract_catch_data(extract_fleet_catch(catch, include = FALSE),
#'   commercial_samples, gear_num = 2, append = TRUE, species_category = 1,
#'   month_fishing_starts = 1, day_fishing_starts = 1, female_only = FALSE)
#' }
extract_catch_data <- function(catch,
                               comm_samples,
                               gear_num = 1,
                               append = FALSE,
                               female_only = FALSE,
                               write_to_file = TRUE,
                               extrap_years = NA,
                               month_fishing_starts = 1,
                               day_fishing_starts = 1,
                               ...){

  options(scipen = 999)

  ct <- tidy_catch(catch,
                   month_fishing_starts = month_fishing_starts,
                   day_fishing_starts = day_fishing_starts,
                   ...) %>%
    select(-species_common_name, -area) %>%
    group_by(year) %>%
    summarize(catch = sum(value) / 1e6) %>%
    ungroup()

  if(female_only){
    props <- props_comm(comm_samples, ...) %>%
      select(-data_source) %>%
      complete(year = seq(min(year), max(year)))

    mean_prop <- props %>% pull(prop_female) %>% mean(na.rm = TRUE)

    props <- props %>%
      mutate(prop_female = ifelse(is.na(prop_female), mean_prop, prop_female))

    ct <- left_join(ct, props, by = "year") %>%
      filter(!is.na(prop_female)) %>%
      mutate(catch = catch * prop_female) %>%
      select(year, catch)
  }

  # Extrapolate the last year's catch if it is part of the way through the year by taking the mean
  # of the proportion of total catch for the year
  if(!is.na(extrap_years)){
    last_year <- tail(sort(unique(ct$year)), 1)
    j <- catch %>%
      filter(year == last_year) %>%
      filter(landed_kg > 0 | discarded_kg > 0)

    last_day_of_catch <- max(j$best_date, na.rm = TRUE)
    last_day_of_catch_month <- month(last_day_of_catch)
    last_day_of_catch_day <- day(last_day_of_catch)
    last_n_years <- (last_year - extrap_years):(last_year - 1)
    previous_year <- last_year - extrap_years - 1

    j <- catch %>%
      filter(year %in% last_n_years) %>%
      set_fishing_year(month_fishing_starts,
                       day_fishing_starts) %>%
      filter(year != previous_year) %>%
      mutate(month = month(best_date),
             day = day(best_date)) %>%
      mutate(yday(ymd(paste0(year, "-", month, "-", day)))) %>%
      group_by(year) %>%
      filter(month <= last_day_of_catch_month & day <= last_day_of_catch_day) %>%
      summarize(catch_so_far = sum(landed_kg + discarded_kg) / 1e6) %>%
      ungroup() %>%
      left_join(ct, by = "year") %>%
      mutate(prop_catch_so_far = catch_so_far / catch)

    avg_prop_caught_so_far <- mean(j$prop_catch_so_far)

    ct <- ct %>%
      mutate(catch = ifelse(year == last_year, catch / avg_prop_caught_so_far, catch))
  }

  # Bind columns in order for data file so it's a simple cut/paste
  yrs <- ct %>% select(year)
  yrs$year <- paste0("   ", yrs$year)
  value <- ct %>% pull(catch)
  value <- format(round(value, 2), digits = 2, nsmall = 2) %>% as_tibble %>% `names<-`("value")
  gear <- rep(paste0(gear_num, "   "), nrow(ct)) %>% as_tibble() %>% `names<-`("gear")
  area <- rep("1   ", nrow(ct)) %>% as_tibble() %>% `names<-`("area")
  group <- rep("1   ", nrow(ct)) %>% as_tibble() %>% `names<-`("group")
  sex <- rep(ifelse(female_only, "2  ", "0  "), nrow(ct)) %>% as_tibble() %>% `names<-`("sex")
  type <- rep("1  ", nrow(ct)) %>% as_tibble() %>% `names<-`("type")
  ct <- bind_cols(yrs, gear, area, group, sex, type, value)

  if(write_to_file){
    nongit_dir <- file.path(dirname(here()), "arrowtooth-nongit")
    dir.create(file.path(nongit_dir, "data-output"), showWarnings = FALSE)
    fn <- file.path(nongit_dir, "data-output/catch.txt")
    write.table(ct, fn, quote = FALSE, row.names = FALSE, append = append)
    message("Catch written to ", fn)
  }else{
    ct
  }
}

#' Extract catch by fleet, where fleet is defined by a vector of vessel names
#'
#' @param catch Output from [gfdata::get_catch()]
#' @param vessel_names A vector of vessel names to include in the catch if `include` is `TRUE`,
#' or exclude if `include` is `FALSE`
#' @param include include or exclude the `vessel_names` from the returned catch data set
#'
#' @return A filtered catch data frame
#' @export
extract_fleet_catch <- function(catch,
                                vessel_names = c("VIKING ENTERPRISE",
                                                 "NORTHERN ALLIANCE",
                                                 "OSPREY NO 1",
                                                 "RAW SPIRIT",
                                                 "PACIFIC LEGACY NO. 1",
                                                 "SUNDEROEY",
                                                 "VIKING ALLIANCE"),
                                include = TRUE){

  if(include){
    catch %>% filter(vessel_name %in% vessel_names)
  }else{
    catch %>% filter(!vessel_name %in% vessel_names)
  }
}

#' Calculate the proportion of the catch caught in the Freezer trawler sector for the `last_n_years`
#'
#' @param ft_catch Output from [extract_fleet_catch()] for Freezer trawlers
#' @param wb_catch Output from [extract_fleet_catch()] for Wet boats
#' @param end_year The last year in the mean calculation
#' @param last_n_years Number of recent years to use in mean proportion calculation
#'
#' @return A single value, the proportion men proportion for the `last_n_years` for Freezer trawlers
#' @export
#'
#' @examples
#' \dontrun{
#' ft_catch <- extract_fleet_catch(catch, include = TRUE)
#' wb_catch <- extract_fleet_catch(catch, include = FALSE)
#' calc_prop_catch_by_fleet(ft_catch, wb_catch, 10)
#' }
calc_prop_catch_by_fleet <- function(ft_catch, wb_catch, end_year = NULL, last_n_years = 10){
  if(is.null(end_year)){
    ft_last <- ft_catch$year %>% unique %>% sort %>% last
    wb_last <- wb_catch$year %>% unique %>% sort %>% last
    if(ft_last != wb_last){
      stop("end_year is NULL and the final year in ft_catch (", ft_last, ") does not equal the final year in ",
           "wb_catch (", wb_last, "). Provide a end_year that is in both data frames", call. = FALSE)
    }
  }
  if(!end_year %in% ft_catch$year || !end_year %in% wb_catch$year){
    stop("end_year (", end_year, ") is not in both ft_catch$year and wb_catch$year", call. = FALSE)
  }
  start_year = end_year - last_n_years + 1
  if(!start_year %in% ft_catch$year || !start_year %in% wb_catch$year){
    stop("last_n_years (", last_n_years, ") produces a start year (", start_year, ") which is not in ",
         "both ft_catch$year and wb_catch$year", call. = FALSE)
  }

  ft <- tidy_catch(ft_catch) %>%
    filter(year %in% start_year:end_year) %>%
    filter(gear %in% c("Discarded", "Midwater trawl")) %>%
    group_by(year) %>%
    summarize(catch_ft = sum(value)) %>%
    ungroup()

  wb <- tidy_catch(wb_catch) %>%
    filter(year %in% start_year:end_year) %>%
    filter(gear %in% c("Discarded", "Midwater trawl")) %>%
    group_by(year) %>%
    summarize(catch_wb = sum(value)) %>%
    ungroup()

  d <- ft %>%
    left_join(wb, by = "year") %>%
    mutate(catch_tot = catch_ft + catch_wb) %>%
    mutate(catch_prop_ft = catch_ft / catch_tot)

  sum(d$catch_prop_ft) / nrow(d)
}