#' Extract the commercial age proportions for pasting into the iSCAM data file.
#'
#' @param catch_sets Data frame as output by [gfdata::get_catch()] or [gfdata::get_survey_sets()]
#' @param samples Data frame as output by [gfdata::get_commercial_samples()] or [gfdata::get_survey_samples()]
#' @param surv_series_name Name of a survey to extract. To extract multiple at once, use [extract_survey_age_comps()].
#' See the values in `survey_abbrev` column of data frame returned by [gfdata::get_survey_samples()] for names
#' @param sex sex code, "M" = male, "F" = female.
#'  For split sex (full age comps for each sex) use sex = c("M", "F")
#' @param ... Arguments to pass to [gfplot::tidy_ages_weighted()]
#'
#' @return Nothing. The output is written to the file
#' @export
#'
#' @examples
#' \dontrun
#' catch <- gfdata::get_catch("arrowtooth flounder")
#' commercial_samples <- gfdata::get_commercial_samples("arrowtooth flounder")
#' extract_age_comps(catch, commercial_samples, spp_cat_code = 1, month_fishing_starts = 2, day_fishing_starts = 21) %>% print(n = 100)
extract_age_comps <- function(catch_sets,
                              samples,
                              type = "commercial",
                              surv_series_name = "SYN QCS",
                              sex = c("M", "F"),
                              write_to_file = TRUE,
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
    gear <- rep(1, nrow(k)) %>% as_tibble() %>% `names<-`("gear")
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
      write.table(jj, fn, quote = FALSE, row.names = FALSE)
      message("Commercial age proportions written to ", fn)
    }else{
      fn <- file.path(nongit_dir, "data-output/survey-age-proportions.txt")
      write.table(jj, fn, quote = FALSE, row.names = FALSE)
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
#' \dontrun
#' survey_sets <- gfdata::get_survey_sets("arrowtooth flounder")
#' survwy_samples <- gfdata::get_survey_samples("arrowtooth flounder")
#' extract_survey_age_comps(catch_sets = survey_sets, samples = survey_samples)
extract_survey_age_comps <- function(surv_series_names = c("SYN QCS", "SYN HS", "SYN WCVI"), write_to_file = FALSE, ...){
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
