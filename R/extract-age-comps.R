#' Extract the female age proportions for pasting into the iSCAM data file.
#'
#' @param catch Data frame as output by [gfdata::get_catch()]
#' @param comm_samples Data frame as output by [gfdata::get_commercial_samples()]
#' @param sex sex code, "M" = male, "F" = female.
#'  For split sex (full age comps for each sex) use sex = c("M", "F")
#'
#' @return Nothing. The output is written to the file
#' @export
#'
#' @examples
#' \dontrun
#' d <- gfdata::get_commercial_samples("arrowtooth flounder")
#' ct <- gfdata::get_catch("arrowtooth flounder")
#' extract_age_comps(ct, d) %>% print(n = 100)
extract_age_comps <- function(catch,
                              comm_samples,
                              sex = c("M", "F")){

  ac <- tidy_ages_weighted(commercial_samples, sample_type = "commercial", dat_catch = catch)

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
    k <- map_df(k, ~{format(.x, digits = 6, nsmall = 6)})
    gear <- rep(1, nrow(k)) %>% as_tibble() %>% `names<-`("gear")
    area <- rep(1, nrow(k)) %>% as_tibble() %>% `names<-`("area")
    group <- rep(1, nrow(k)) %>% as_tibble() %>% `names<-`("group")
    sex <- rep(ifelse(.x == "M", 1, 2), nrow(k)) %>% as_tibble() %>% `names<-`("sex")
    k <- bind_cols(yrs, gear, area, group, sex, k, samps)
    k %>% mutate(total = paste0("#", total))
  }) %>% bind_rows

  nongit_dir <- file.path(dirname(here()), "arrowtooth-nongit")
  dir.create(file.path(nongit_dir, "data-output"), showWarnings = FALSE)
  fn <- file.path(nongit_dir, "data-output/commercial-age-proportions.txt")
  write.table(jj, fn, quote = FALSE, row.names = FALSE)
  message("Commercial age proportions written to ", fn)
}