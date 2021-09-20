#' Extract the female survey index data for pasting into the iSCAM data file.
#' The 'Discard CPUE' index must be multiplied by the proportions female from the commerciual fishery
#' so `comm_samples` is required
#'
#' @details The CV can be calculated as CV = SD / MEAN or from the Standard error: CV = sqrt(exp(SE^2) - 1)
#'
#' @rdname props_surv
#' @param survey_index Survey index data frame as output by [gfdata::get_survey_index()]
#' @param iphc The IPHC index as read in from iphc-survey-index.rds
#' @param discard_cpue The discard CPUE index as read from
#' cpue-predictions-arrowtooth-flounder-modern-3CD5ABCDE-discard-july-26-feb-fishing-year
#' @param stitched_syn The stitched synoptic index as read in from stitched-syn-index.rds
#' @param write_to_file If `TRUE`, write the output to the file. If `FALSE`, return the data frame
#' @param append If `TRUE`, append the output to the file. If `FALSE`, overwrite the file
#' @param ... Arguments passed to [props_comm()]
#'
#' @return A list of survey indices for pasting into a iSCAM data file
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_at
#' @importFrom readr read_csv
#' @importFrom stats sd
#' @export
#'
#' @examples
#' \dontrun{
#' si <- gfdata::get_survey_index("arrowtooth flounder")
#' extract_survey_indices (survey_index, start_year = 1996, end_year = 2019, species_category = 1)
#' }
extract_survey_indices <- function(survey_index,
                                   surv_series = c(2, 3, 4, 5, 17),
                                   surv_series_names = c("SYN QCS", "OTHER HS MSA", "SYN HS", "SYN WCVI", "SYN WCHG"),
                                   iphc = NULL,
                                   discard_cpue = NULL,
                                   stitched_syn = NULL,
                                   write_to_file = TRUE,
                                   append = FALSE,
                                   ...){

  if(!is.null(iphc)){
    iphc <- iphc %>%
      transmute(survey = "IPHC FISS",
                year = year,
                index = I_t20SampleMean)
    se <- sd(iphc$index) / sqrt(nrow(iphc))
    iphc <- iphc %>% mutate(wt = index / se)
  }

  # Discard CPUE load and add wt (1/relerr)
  if(!is.null(discard_cpue)){
    discard_cpue <- discard_cpue %>%
      filter(formula_version == "Full standardization") %>%
      transmute(survey = "CPUE discard",
                year = year,
                index = est,
                wt = 1 / sqrt(exp(se_link ^ 2) - 1))
  }

  # Stitched synoptic surveys
  if(!is.null(stitched_syn)){
    stitched_syn <- stitched_syn %>%
      transmute(survey = "Stitched Synoptics",
                year = year,
                index = est / 1e6,
                wt = 1 / sqrt(exp(se ^ 2) - 1))
  }

  surv_indices <- survey_index %>%
    filter(survey_abbrev %in% surv_series_names) %>%
    transmute(survey = survey_abbrev,
              year = year,
              index = biomass / 1e6, wt = 1 / re) %>%
    bind_rows(iphc) %>%
    bind_rows(discard_cpue) %>%
    bind_rows(stitched_syn)

  # Format for digits
  surv_indices <- map_at(surv_indices, c("index", "wt"), ~{
    format(round(.x, 2), digits = 2, nsmall = 2)
    }) %>%
    map_df(~{.x})

  # Organize into iSCAM data file order
  survey_years_index <- surv_indices %>% select(survey, year, index)
  survey_years_index$year <- paste0("  ", survey_years_index$year)
  survey_years_index$index <- paste0("  ", survey_years_index$index)

  wt <- surv_indices %>% select(wt)
  wt$wt <- paste0("  ", wt$wt)

  # Add gear number
  survey_years_index <- survey_years_index %>%
    mutate(mtc = match(surv_indices$survey, surv_series_names) + 1,
           gear = ifelse(is.na(mtc), paste0("    ", 1), paste0("    ", mtc))) %>%
    select(-mtc)

  area <- rep("   1", nrow(surv_indices)) %>% as_tibble() %>% `names<-`("area")
  group <- rep("   1", nrow(surv_indices)) %>% as_tibble() %>% `names<-`("group")
  sex <- rep("    0", nrow(surv_indices)) %>% as_tibble() %>% `names<-`("sex")
  timing <- rep(" 0.0", nrow(surv_indices)) %>% as_tibble() %>% `names<-`("timing")
  surv_indices <- bind_cols(survey_years_index, area, group, sex, wt, timing) %>%
    arrange(gear, survey, year)

  if(write_to_file){
    nongit_dir <- file.path(dirname(here()), "arrowtooth-nongit")
    dir.create(file.path(nongit_dir, "data-output"), showWarnings = FALSE)
    fn <- file.path(nongit_dir, "data-output/survey-indices.txt")
    write.table(surv_indices, fn, quote = FALSE, row.names = FALSE, append = append)
    message("Survey indices written to ", fn)
  }else{
    surv_indices
  }
}
