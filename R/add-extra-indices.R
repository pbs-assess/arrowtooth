#' Add survey indices from files into the survey index object
#'
#' @param surv_index A survey index object as returned by [gfdata::get_survey_index()]
#' @param iphc The IPHC index data
#' @param discard_cpue The Discard CPUE index data
#' @param stitched_syn The Stitched Synoptics index data
#'
#' @return A survey index object as returned by [gfdata::get_survey_index()], with the additional indices added
#' @export
add_extra_indices <- function(surv_index = NULL,
                              iphc = NULL,
                              discard_cpue = NULL,
                              stitched_syn = NULL){
  stopifnot(!is.null(surv_index))

  if(!is.null(iphc)){
    iphc <- iphc %>%
      transmute(year,
                biomass = I_t20BootMean,
                lowerci = I_t20BootLow,
                upperci = I_t20BootHigh,
                re = NA,
                num_sets = Sets,
                num_pos_sets = num_pos20,
                survey_series_id = 14,
                survey_abbrev = "IPHC FISS",
                survey_series_desc = "International Pacific Halibut Commission Fishery-Independent Setline Survey")
    if(ncol(surv_index) != ncol(iphc)){
      stop("Check the number of columns in surv_index, it does not match the iphc extraction", call. = FALSE)
    }
    # Remove IPHC FISS and add new one (Done by Andy July 2021).
    # This code be removed once that is replaced in the DB officially
    surv_index <- surv_index %>%
      filter(survey_abbrev != "IPHC FISS")
    surv_index <- surv_index %>% bind_rows(iphc)
  }
  if(!is.null(discard_cpue)){
    discard_cpue <- discard_cpue %>%
      filter(formula_version == "Full standardization") %>%
      transmute(year,
                biomass = est,
                lowerci = lwr,
                upperci = upr,
                re = NA,
                num_sets = NA,
                num_pos_sets = NA,
                survey_series_id = NA,
                survey_abbrev = "DCPUE",
                survey_series_desc = "Discard CPUE")
    if(ncol(surv_index) != ncol(discard_cpue)){
      stop("Check the number of columns in surv_index, it does not match the discard_cpue extraction", call. = FALSE)
    }
    surv_index <- surv_index %>% bind_rows(discard_cpue)
  }
  if(!is.null(stitched_syn)){
    stitched_syn <- stitched_syn %>%
      as_tibble() %>%
      transmute(year,
                biomass = est,
                lowerci = lwr,
                upperci = upr,
                re = NA,
                num_sets = NA,
                num_pos_sets = NA,
                survey_series_id = NA,
                survey_abbrev = "STITCH",
                survey_series_desc = "Stitched Synoptics")
    if(ncol(surv_index) != ncol(stitched_syn)){
      stop("Check the number of columns in surv_index, it does not match the stitched_syn extraction", call. = FALSE)
    }
    surv_index <- surv_index %>% bind_rows(stitched_syn)
  }
  surv_index
}