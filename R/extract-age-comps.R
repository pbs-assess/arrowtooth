#' Extract the female age proportions for pasting into the iSCAM data file.
#'
#' @param catch Data frame as output by [gfdata::get_catch()]
#' @param comm_samples Data frame as output by [gfdata::get_commercial_samples()]
#' @param fn A file name to write the output to. The output is ASCII text written
#'  using [utils::write.table()]
#'
#' @return Nothiung. The output is written to the file
#' @export
#'
#' @examples
#' \dontrun
#' d <- gfdata::get_commercial_samples("arrowtooth flounder")
#' ct <- gfdata::get_catch("arrowtooth flounder")
#' extract_age_comps(ct, d) %>% print(n = 100)
extract_age_comps <- function(catch,
                              comm_samples,
                              fn){

  ac <- tidy_ages_weighted(commercial_samples, sample_type = "commercial", dat_catch = catch)

  j <- ac %>%
    filter(sex == "F") %>%
    select(-species_common_name, -survey_abbrev, -sex) %>%
    arrange(year, age) %>%
    complete(age = 1:20) %>%
    pivot_wider(names_from = "age", values_from = proportion) %>%
    arrange(year) %>%
    filter(!is.na(year))

  j <- j[order(j$year),]
  j[is.na(j)] <- 0

  # Plus group
  plus <- j[, grepl("2[0-9]+", names(j))] %>% rowSums %>% as_tibble() %>% `names<-`("20")
  without_plus <- j[, !grepl("2[0-9]+", names(j))]
  j <- bind_cols(without_plus, plus)

  # Bind columns in order for data file so it's a simple cut/paste
  yrs <- j %>% select(year)
  samps <- j %>% select(total)
  j <- j %>% select(-year, -total)
  j <- map_df(j, ~{format(.x, digits = 6, nsmall = 6)})
  gear <- rep(1, nrow(j)) %>% as_tibble() %>% `names<-`("gear")
  area <- rep(1, nrow(j)) %>% as_tibble() %>% `names<-`("area")
  group <- rep(1, nrow(j)) %>% as_tibble() %>% `names<-`("group")
  sex <- rep(0, nrow(j)) %>% as_tibble() %>% `names<-`("sex")
  j <- bind_cols(yrs, gear, area, group, sex, j, samps)
  j <- j %>% mutate(total = paste0("#", total))

  write.table(j, fn, quote = FALSE, row.names = FALSE)
  message("Age comps written to ", fn)
}