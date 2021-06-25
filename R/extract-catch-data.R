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
#' @param ... Arguments to be passed on to [props_comm()]
#'
#' @return A data frame you can cut and paste into the iSCAM data file
#' @export
#' @importFrom gfplot tidy_catch
#' @examples
#' \dontrun
#' d <- gfdata::get_commercial_samples("arrowtooth flounder")
#' ct <- gfdata::get_catch("arrowtooth flounder")
#' extract_catch_data(ct, d, species_category = 1, end_year = 2019) %>% print(n=100)
extract_catch_data <- function(catch,
                               comm_samples,
                               ...){

  ct <- tidy_catch(catch) %>%
    select(-species_common_name, -area) %>%
    group_by(year) %>%
    summarize(value = sum(value)) %>%
    ungroup()

  props <- props_comm(comm_samples, ...) %>%
    select(-data_source) %>%
    complete(year = seq(min(year), max(year)))

  mean_prop <- props %>% pull(prop_female) %>% mean(na.rm = T)

  props <- props %>%
    mutate(prop_female = ifelse(is.na(prop_female), mean_prop, prop_female))

  left_join(ct, props, by = "year") %>%
    filter(!is.na(prop_female)) %>%
    rename(catch = value) %>%
    mutate(catch = catch / 1e6) %>%
    mutate(female_catch = catch * prop_female) %>%
    select(year, female_catch)
}
