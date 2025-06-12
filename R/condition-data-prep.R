#' Prepare data for body condition modelling
#'
#' @param d A list, as output from [gfdata::get_data()]
#' @param survey_abbrev A survey abbreviation as found in
#' `d$survey_sets$survey_abbrev`
#'
#' @return A data frame of survey sets
#' @export
condition_data_prep <- function(d,
                                survey_abbrev = c("SYN QCS",
                                                  "SYN HS",
                                                  "SYN WCVI",
                                                  "SYN WCHG")){

  surv_sets <- d$survey_sets
  surv_samps <- d$survey_samples |>
    select(fishing_event_id,
           year,
           length,
           sex,
           age,
           weight,
           usability_code,
           specimen_id)

  d <- surv_samps |>
    left_join(surv_sets)

  d
}
