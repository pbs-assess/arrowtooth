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
#' @param ... Arguments to be passed on to [props_comm()] and [gfplot::tidy_catch()]
#'
#' @return A data frame you can cut and paste into the iSCAM data file
#' @export
#' @importFrom gfplot tidy_catch
#' @examples
#' \dontrun{
#' Freezer trawlers:
#' extract_catch_data(extract_fleet_catch(catch, include = TRUE), commercial_samples, gear_num = 1, append = FALSE,
#' species_category = 1, estart_year = 1996, nd_year = 2019, month_fishing_starts = 1, day_fishing_starts = 1, female_only = TRUE)
#' Non-Freezer trawlers:
#' extract_catch_data(extract_fleet_catch(catch, include = FALSE), commercial_samples, gear_num = 2, append = TRUE,
#' species_category = 1, start_year = 1996, end_year = 2019, month_fishing_starts = 1, day_fishing_starts = 1, female_only = TRUE)
#' }
extract_catch_data <- function(catch,
                               comm_samples,
                               gear_num = 1,
                               append = FALSE,
                               female_only = FALSE,
                               ...){

  options(scipen = 999)
  ct <- tidy_catch(catch, ...) %>%
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

  nongit_dir <- file.path(dirname(here()), "arrowtooth-nongit")
  dir.create(file.path(nongit_dir, "data-output"), showWarnings = FALSE)
  fn <- file.path(nongit_dir, "data-output/catch.txt")
  write.table(ct, fn, quote = FALSE, row.names = FALSE, append = append)
  message("Catch written to ", fn)
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
                                                 "OSPREY NO. 1",
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