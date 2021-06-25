#' Extract the female survey index data for pasting into the iSCAM data file.
#'
#' @rdname props_surv
#' @param survey_index Survey index data frame as output by [gfdata::get_survey_index()]
#'
#' @return A list of survey indices for pasting into a iSCAM data file
#' @export
#'
#' @examples
#' \dontrun
#' si <- gfdata::get_survey_index("arrowtooth flounder")
#' ss <- gfdata::get_survey_sets("arrowtooth flounder")
#' ssa <- gfdata::get_survey_samples("arrowtooth flounder")
#' extract_catch_data(survey_index = si, survey_sets = ss, survey_samples = ssa)
extract_survey_indices <- function(surv_series = 1:4,
                                   surv_names = c("SYN QCS", "OTHER HS MSA", "SYN HS", "SYN WCVI"),
                                   survey_index,
                                   survey_sets,
                                   survey_samples){

  # IPHC and CPUE discard
  #iphc <- readRDS(here("presentations/pre-review/iphc-cached-data/arrowtooth-flounder-results.rds"))
  #iphc <- iphc$series_ABCD_full$ser_longest
  #cpue_discard <- readr::read_csv(here("presentations/responses-to-twg/cpue-cached-data/cpue-predictions-arrowtooth-flounder-modern-3CD5ABCDE.csv"))

  surv_indices <- survey_index %>%
    filter(survey_abbrev %in% surv_names) %>%
    transmute(survey = survey_abbrev, year = year, index = biomass / 1e6, wt = 1 / re)

  props <- props_surv(surv_series,
                      surv_names,
                      survey_sets,
                      survey_samples)

  props <- props %>%
    rename(survey = data_source)

  k <- left_join(surv_indices, props, by = c("survey", "year")) %>%
    group_by(survey) %>%
    group_split()

  k %>% map(~{
    mean_prop <- mean(.x$prop_female, na.rm = TRUE)
    .x <- .x %>%
      mutate(prop_female = ifelse(is.na(prop_female), mean_prop, prop_female)) %>%
      mutate(female_catch = index * prop_female, female_wt = wt * prop_female) %>%
      select(survey, year, female_catch, female_wt)
    .x
  })
}
