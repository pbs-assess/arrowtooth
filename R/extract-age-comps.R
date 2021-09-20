#' Extract the commercial age proportions for pasting into the iSCAM data file.
#'
#' @param catch_sets Data frame as output by [gfdata::get_catch()] or [gfdata::get_survey_sets()]
#' @param samples Data frame as output by [gfdata::get_commercial_samples()] or [gfdata::get_survey_samples()]
#' @param gear_num Number of gear to be written in output file
#' @param surv_series_name Name of a survey to extract. To extract multiple at once, use [extract_survey_age_comps()].
#' See the values in `survey_abbrev` column of data frame returned by [gfdata::get_survey_samples()] for names
#' @param sex sex code, "M" = male, "F" = female.
#'  For split sex (full age comps for each sex) use sex = c("M", "F")
#' @param write_to_file If `TRUE` write the output to the file. If `FALSE` return it
#' @param append If `TRUE`, append the output to the file. If `FALSE`, overwrite the file
#' @param ... Arguments to pass to [gfplot::tidy_ages_weighted()]
#'
#' @return Nothing. The output is written to the file
#' @importFrom tidyr complete
#' @export
#'
#' @examples
#' \dontrun{
#' catch <- gfdata::get_catch("arrowtooth flounder")
#' commercial_samples <- gfdata::get_commercial_samples("arrowtooth flounder")
#' catch_ft <- extract_fleet_catch(catch)
#' catch_nonft <- extract_fleet_catch(catch, include = FALSE)
#' comm_ft <- extract_fleet_samples(commercial_samples)
#' comm_nonft <- extract_fleet_samples(commercial_samples, include = FALSE)
#' extract_age_comps(catch_ft, comm_ft, gear_num = 1, spp_cat_code = 1, month_fishing_starts = 2, day_fishing_starts = 21, append = FALSE)
#' extract_age_comps(catch_nonft, comm_nonft, gear_num = 2, spp_cat_code = 1, month_fishing_starts = 2, day_fishing_starts = 21, append = TRUE)
#' extract_age_comps(survey_sets, survey_samples, type = "survey", gear_num = 6, surv_series_name = "SYN WCHG")
#' }
extract_age_comps <- function(catch_sets,
                              samples,
                              type = "commercial",
                              gear_num = 1,
                              surv_series_name = "SYN QCS",
                              sex = c("M", "F"),
                              write_to_file = TRUE,
                              append = FALSE,
                              ...){

  if(type == "commercial"){
    ac <- tidy_ages_weighted(samples, sample_type = "commercial", dat_catch = catch_sets, ...)
  }else if(type == "survey"){
    ac <- tidy_ages_weighted(samples, sample_type = "survey", dat_survey_sets = catch_sets, ...) %>%
      filter(survey_abbrev == surv_series_name)
  }else{
    stop("type must be 'commercial' or 'survey'", call. = FALSE)
  }

  j <- ac %>%
    select(-species_common_name, -survey_abbrev)

  jj <- map(sex, ~{
    k <- j %>%
      filter(sex == .x) %>%
      arrange(year, age) %>%
      complete(age = 1:20) %>%
      select(-sex) %>%
      pivot_wider(names_from = "age", values_from = proportion) %>%
      arrange(year) %>%
      filter(!is.na(year))
    k <- k[order(j$year),]
    k[is.na(k)] <- 0

    # Plus group
    plus <- k[, grepl("2[0-9]+", names(k))] %>% rowSums %>% as_tibble() %>% `names<-`("20")
    without_plus <- k[, !grepl("2[0-9]+", names(k))]
    k <- bind_cols(without_plus, plus) %>%
      filter(year > 0)

    # Bind columns in order for data file so it's a simple cut/paste
    yrs <- k %>% select(year)
    samps <- k %>% select(total)
    k <- k %>% select(-year, -total)
    k <- map_df(k, ~{format(round(.x, 6), digits = 6, nsmall = 6)})
    gear <- rep(gear_num, nrow(k)) %>% as_tibble() %>% `names<-`("gear")
    area <- rep(1, nrow(k)) %>% as_tibble() %>% `names<-`("area")
    group <- rep(1, nrow(k)) %>% as_tibble() %>% `names<-`("group")
    sex <- rep(ifelse(.x == "M", 1, 2), nrow(k)) %>% as_tibble() %>% `names<-`("sex")
    k <- bind_cols(yrs, gear, area, group, sex, k, samps)
    k %>% mutate(total = paste0("#", total))
  }) %>% bind_rows

  if(write_to_file){
    nongit_dir <- file.path(dirname(here()), "arrowtooth-nongit")
    dir.create(file.path(nongit_dir, "data-output"), showWarnings = FALSE)
    if(type == "commercial"){
      fn <- file.path(nongit_dir, "data-output/commercial-age-proportions.txt")
      write.table(jj, fn, quote = FALSE, row.names = FALSE, append = append)
      message("Commercial age proportions written to ", fn)
    }else{
      fn <- file.path(nongit_dir, "data-output/survey-age-proportions.txt")
      write.table(jj, fn, quote = FALSE, row.names = FALSE, append = append)
      message("Survey age proportions written to ", fn)
    }
  }else{
    jj
  }
}

#' Extract the survey age proportions for pasting into the iSCAM data file.
#'
#' @param ... Arguments to pass to [extract_age_comps()]
#' @param surv_series_names See the values in `survey_abbrev` column of data frame returned
#'  by [gfdata::get_survey_samples()] for names of surveys to include
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' survey_sets <- gfdata::get_survey_sets("arrowtooth flounder")
#' survwy_samples <- gfdata::get_survey_samples("arrowtooth flounder")
#' extract_survey_age_comps(catch_sets = survey_sets, samples = survey_samples)
#' }
extract_survey_age_comps <- function(surv_series_names = c("SYN QCS", "OTHER HS MSA", "SYN HS", "SYN WCVI"), write_to_file = TRUE, ...){
  j <- map2(surv_series_names, seq_along(surv_series_names), function(x = .x, y = .y, ...){
    extract_age_comps(type = "survey", surv_series_name = x, write_to_file = FALSE, ...) %>%
      mutate(gear = y + 1)
  }, ...) %>%
    bind_rows()

  if(write_to_file){
    nongit_dir <- file.path(dirname(here()), "arrowtooth-nongit")
    dir.create(file.path(nongit_dir, "data-output"), showWarnings = FALSE)
    fn <- file.path(nongit_dir, "data-output/survey-age-proportions.txt")
    write.table(j, fn, quote = FALSE, row.names = FALSE)
    message("Survey age proportions written to ", fn)
  }else{
    j
  }
}

#' Extract commercial samples by fleet, where fleet is defined by a vector of vessel ids
#'
#' @param comm Output from [gfdata::get_commercial_samples()]
#' @param vessel_ids A vector of vessel IDs to include in the commercial samples if `include` is `TRUE`,
#' or exclude if `include` is `FALSE`
#' @param include include or exclude the `vessel_ids` from the returned commercial samples data set
#'
#' @return A filtered commercial samples data frame
#' @export
extract_fleet_samples <- function(comm,
                                  vessel_ids = c(568,   # VIKING ENTERPRISE
                                                 592,   # NORTHERN ALLIANCE
                                                 569,   # OSPREY NO. 1
                                                 595,   # RAW SPIRIT
                                                 608,   # PACIFIC LEGACY NO. 1
                                                        # SUNDEROEY (not yet in GFBIO - July 2021)
                                                 1727), # VIKING ALLIANCE
                                  include = TRUE){

  if(include){
    comm %>% filter(vessel_id %in% vessel_ids)
  }else{
    comm %>% filter(!vessel_id %in% vessel_ids)
  }
}