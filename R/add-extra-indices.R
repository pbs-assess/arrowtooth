#' Add survey indices from files into the survey index object
#'
#' @param surv_index A survey index object as returned by [gfdata::get_survey_index()]
#' @param data_path The full path where the data files can be found
#' @param iphc_rds_fn The name of the IPHC RDS file containing index data
#' @param discard_cpue_csv_fn The name of the Discard CPUE CSV file containing index data
#' @param stitched_syn_rds_fn The name of the Stitched Synoptics RDS file containing index data
#'
#' @return A survey index object as returned by [gfdata::get_survey_index()], with the additional indices added
#' @export
add_extra_indices <- function(surv_index = NULL,
                              data_path = NULL,
                              iphc_rds_fn = NULL,
                              discard_cpue_csv_fn = NULL,
                              stitched_syn_rds_fn = NULL){
  stopifnot(!is.null(surv_index))
  stopifnot(!is.null(data_path))

  if(!is.null(iphc_rds_fn)){
    iphc_file <- file.path(data_path, iphc_rds_fn)
    if(!file.exists(iphc_file)){
      stop("File ", iphc_file, " does not exist.", call. = FALSE)
    }
    iphc <- readRDS(iphc_file)$series_ABCD_full$ser_longest %>%
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
  if(!is.null(discard_cpue_csv_fn)){
    cpue_discard_file <- file.path(data_path, discard_cpue_csv_fn)
    if(!file.exists(cpue_discard_file)){
      stop("File ", cpue_discard_file, " does not exist.", call. = FALSE)
    }
    cpue_discard <- read_csv(cpue_discard_file) %>%
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
    if(ncol(surv_index) != ncol(cpue_discard)){
      stop("Check the number of columns in surv_index, it does not match the cpue_discard extraction", call. = FALSE)
    }
    surv_index <- surv_index %>% bind_rows(cpue_discard)
  }
  if(!is.null(stitched_syn_rds_fn)){
    stitched_syn_file <- file.path(data_path, stitched_syn_rds_fn)
    if(!file.exists(stitched_syn_file)){
      stop("File ", stitched_syn_file, " does not exist.", call. = FALSE)
    }
    stitched_syn <- readRDS(stitched_syn_file) %>%
      as_tibble() %>%
      transmute(year,
                biomass = est / 1e6,
                lowerci = lwr,
                upperci = upr,
                re = NA,
                num_sets = NA,
                num_pos_sets = NA,
                survey_series_id = NA,
                survey_abbrev = "SYN STCH",
                survey_series_desc = "Stitched Synoptics")
    if(ncol(surv_index) != ncol(stitched_syn)){
      stop("Check the number of columns in surv_index, it does not match the stitched_syn extraction", call. = FALSE)
    }
    surv_index <- surv_index %>% bind_rows(stitched_syn)
  }
  surv_index
}