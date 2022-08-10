#' Calculate the proportion of the catch caught in the Freezer trawler sector
#' for the `last_n_years`
#'
#' @param ft_catch Output from [extract_fleet_catch()] for Freezer trawlers
#' @param wb_catch Output from [extract_fleet_catch()] for Wet boats
#' @param end_year The last year in the mean calculation
#' @param last_n_years Number of recent years to use in mean
#' proportion calculation
#'
#' @return A single value, the proportion men proportion for the
#' `last_n_years` for Freezer trawlers
#' @export
#'
#' @examples
#' \dontrun{
#' ft_catch <- extract_fleet_catch(catch, include = TRUE)
#' wb_catch <- extract_fleet_catch(catch, include = FALSE)
#' calc_prop_catch_by_fleet(ft_catch, wb_catch, 10)
#' }
calc_prop_catch_by_fleet <- function(ft_catch,
                                     wb_catch,
                                     end_year = NULL,
                                     last_n_years = 10){
  if(is.null(end_year)){
    ft_last <- ft_catch$year %>% unique %>% sort %>% last
    wb_last <- wb_catch$year %>% unique %>% sort %>% last
    if(ft_last != wb_last){
      stop("end_year is NULL and the final year in ft_catch (", ft_last,
           ") does not equal the final year in ",
           "wb_catch (", wb_last, "). Provide a end_year that is in both ",
           "data frames", call. = FALSE)
    }
  }
  if(!end_year %in% ft_catch$year || !end_year %in% wb_catch$year){
    stop("end_year (", end_year, ") is not in both ft_catch$year and ",
         "wb_catch$year", call. = FALSE)
  }
  start_year = end_year - last_n_years + 1
  if(!start_year %in% ft_catch$year || !start_year %in% wb_catch$year){
    stop("last_n_years (", last_n_years, ") produces a start year (",
         start_year, ") which is not in ",
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

