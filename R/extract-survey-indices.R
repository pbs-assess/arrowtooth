#' Extract the female survey index data for pasting into the iSCAM data file.
#' The 'Discard CPUE' index must be multiplied by the proportions female feom the commerciual fishery
#' so `comm_samples` is required
#'
#' @details The CV can be calculated as CV = SD / MEAN or from the Standard error: CV = sqrt(exp(SE^2) - 1)
#'
#' @rdname props_surv
#' @param survey_index Survey index data frame as output by [gfdata::get_survey_index()]
#' @param comm_samples Data frame as output by [gfdata::get_commercial_samples()]
#' @param ... Arguments passed to [props_comm()]
#'
#' @return A list of survey indices for pasting into a iSCAM data file
#' @importFrom dplyr bind_rows
#' @importFrom readr read_csv
#' @export
#'
#' @examples
#' \dontrun
#' si <- gfdata::get_survey_index("arrowtooth flounder")
#' ss <- gfdata::get_survey_sets("arrowtooth flounder")
#' ssa <- gfdata::get_survey_samples("arrowtooth flounder")
#' comm <- gfdata::get_commercial_samples("arrowtooth flounder")
#' extract_survey_indices (survey_index, survey_sets, survey_samples, commercial_samples,
#'  start_year = 1996, end_year = 2019, species_category = 1)
extract_survey_indices <- function(survey_index,
                                   survey_sets,
                                   survey_samples,
                                   comm_samples,
                                   surv_series = 1:4,
                                   surv_names = c("SYN QCS", "OTHER HS MSA", "SYN HS", "SYN WCVI"),
                                   ...){

  nongit_dir <- file.path(dirname(here()), "arrowtooth-nongit")

  # IPHC load and add wt (1/relerr)
  iphc_file <- file.path(nongit_dir, "data/iphc-survey-index.rds")
  if(!file.exists(iphc_file)){
    stop("File ", iphc_file, " does not exist.", call. = FALSE)
  }
  iphc <- readRDS(iphc_file)$series_ABCD_full$ser_longest %>%
    transmute(survey = "IPHC FISS",
              year = year,
              index = I_t20SampleMean)
  se <- sd(iphc$index) / sqrt(nrow(iphc))
  iphc <- iphc %>% mutate(wt = index / se)

  # Discard CPUE load and add wt (1/relerr)
  cpue_discard_file <- file.path(nongit_dir, "data/cpue-predictions-arrowtooth-flounder-modern-3CD5ABCDE.csv")
  if(!file.exists(cpue_discard_file)){
    stop("File ", cpue_discard_file, " does not exist.", call. = FALSE)
  }
  cpue_discard <- read_csv(cpue_discard_file) %>%
    filter(formula_version == "Full standardization") %>%
    transmute(survey = "CPUE discard",
              year = year,
              index = est,
              wt = 1 / sqrt(exp(se_link ^ 2) - 1))

  # Stitched synoptic surveys
  stitched_syn_file <- file.path(nongit_dir, "data/stitched-syn-index.rds")
  if(!file.exists(stitched_syn_file)){
    stop("File ", stitched_syn_file, " does not exist.", call. = FALSE)
  }
  stitched_syn <- readRDS(stitched_syn_file) %>%
    transmute(survey = "Stitched Synoptics",
              year = year,
              index = est / 1e6,
              wt = 1 / sqrt(exp(se ^ 2) - 1))

  surv_indices <- survey_index %>%
    filter(survey_abbrev %in% surv_names) %>%
    transmute(survey = survey_abbrev,
              year = year,
              index = biomass / 1e6, wt = 1 / re) %>%
    bind_rows(iphc) %>%
    bind_rows(cpue_discard) %>%
    bind_rows(stitched_syn)

  # Need commercial proportions female for CPUE discard series
  props_c <- props_comm(comm_samples, ...) %>%
    select(-data_source) %>%
    complete(year = seq(min(year), max(year)))
  mean_props_c <- mean(props_c$prop_female, na.rm = TRUE)
  props_c <- props_c %>%
    mutate(prop_female = ifelse(is.na(prop_female), mean_props_c, prop_female)) %>%
    mutate(survey = "CPUE discard")

  props <- props_surv(surv_series,
                      surv_names,
                      survey_sets,
                      survey_samples) %>%
    rename(survey = data_source)

  props <- props %>% bind_rows(props_c)

  # Use mean of proportions female from all surveys and commercial for IPHC index
  mean_prop_iphc <- mean(props$prop_female)
  iphc_yrs <- surv_indices %>% filter(survey == "IPHC FISS") %>% pull(year) %>% as_tibble() %>% `names<-`("year")
  iphc_prop_female <- rep(mean_prop_iphc, nrow(iphc_yrs)) %>% as_tibble() %>% `names<-`("prop_female")
  iphc_survey <- rep("IPHC FISS", nrow(iphc_yrs)) %>% as_tibble() %>% `names<-`("survey")
  props_iphc <- bind_cols(iphc_yrs, iphc_prop_female, iphc_survey)

  props <- props %>% bind_rows(props_iphc)

  # Use mean of all proportions female from all Synoptic surveys for stitched index
  mean_prop_syn <- props %>% filter(grepl("SYN", survey)) %>% pull(prop_female) %>% mean
  stitched_yrs <- surv_indices %>% filter(survey == "Stitched Synoptics") %>% pull(year) %>% as_tibble() %>% `names<-`("year")
  stitched_prop_female <- rep(mean_prop_syn, nrow(stitched_yrs)) %>% as_tibble() %>% `names<-`("prop_female")
  stitched_survey <- rep("Stitched Synoptics", nrow(stitched_yrs)) %>% as_tibble() %>% `names<-`("survey")
  props_stitched <- bind_cols(stitched_yrs, stitched_prop_female, stitched_survey)

  props <- props %>% bind_rows(props_stitched)

  j <- left_join(surv_indices, props, by = c("survey", "year")) %>%
    group_by(survey) %>%
    group_split()

  j <- j %>% map(~{
    mean_prop <- mean(.x$prop_female, na.rm = TRUE)
    .x <- .x %>%
      mutate(prop_female = ifelse(is.na(prop_female), mean_prop, prop_female)) %>%
      mutate(female_catch = index * prop_female, female_wt = wt * prop_female) %>%
      select(survey, year, female_catch, female_wt)

    .x
  }) %>%
    bind_rows()

  dir.create(file.path(nongit_dir, "data-output"), showWarnings = FALSE)
  fn <- file.path(nongit_dir, "data-output/survey-indices.txt")
  write.table(j, fn, quote = FALSE, row.names = FALSE)
  message("Survey indices written to ", fn)
}
