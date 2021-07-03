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
#' @param female_only If `TRUE`, indices and wts will be multiplied by the proportion female
#' @param ... Arguments to be passed on to [props_comm()] and [gfplot::tidy_catch()]
#'
#' @return A data frame you can cut and paste into the iSCAM data file
#' @export
#' @importFrom gfplot tidy_catch
#' @examples
#' \dontrun
#' ct <- gfdata::get_catch("arrowtooth flounder")
#' d <- gfdata::get_commercial_samples("arrowtooth flounder")
#' extract_catch_data(ct, d, species_category = 1, end_year = 2019, month_fishing_starts = 2, day_fishing_starts = 21) %>% print(n=100)
extract_catch_data <- function(catch,
                               comm_samples,
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
  gear <- rep("1   ", nrow(ct)) %>% as_tibble() %>% `names<-`("gear")
  area <- rep("1   ", nrow(ct)) %>% as_tibble() %>% `names<-`("area")
  group <- rep("1    ", nrow(ct)) %>% as_tibble() %>% `names<-`("group")
  sex <- rep(ifelse(female_only, "2  ", "0  "), nrow(ct)) %>% as_tibble() %>% `names<-`("sex")
  type <- rep("1  ", nrow(ct)) %>% as_tibble() %>% `names<-`("type")
  ct <- bind_cols(yrs, gear, area, group, sex, type, value)

  nongit_dir <- file.path(dirname(here()), "arrowtooth-nongit")
  dir.create(file.path(nongit_dir, "data-output"), showWarnings = FALSE)
  fn <- file.path(nongit_dir, "data-output/catch.txt")
  write.table(ct, fn, quote = FALSE, row.names = FALSE)
  message("Catch written to ", fn)
}
