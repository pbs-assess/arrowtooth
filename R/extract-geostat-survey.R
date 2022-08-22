#' Extract data from geostat RDS file for input into iSCAM data file
#'
#' @param fn The geostat RDS filename
#' @param decimals Number of decimal points to round to for index and weight
#' @param gear_num The number to put in the gear column of the output data
#' table
#' @param area The area number to put in the area column of the output data
#' table
#' @param group The group number to put in the group column of the output data
#' table
#' @param sex The sex number to put in the sex column of the output data table
#' @param survey_timing The survey_timing value to put in the survey_timing
#' column of the output data table
#' @param out_fn The output filename. The file will be created in the same
#' directory as the input file
#'
#' @return Nothing
#' @export
extract_geostat_survey <- function(fn,
                                   decimals = 2,
                                   gear_num = 1,
                                   area = 1,
                                   group = 1,
                                   sex = 0,
                                   survey_timing = 0.0,
                                   out_fn = file.path(dirname(fn),
                                                      paste0("geostat-survey-",
                                                             gear_num,
                                                             "-",
                                                             as.character(Sys.Date()),
                                                             ".txt"))){

  j <- readRDS(fn) |>
    as_tibble() |>
    select(year, log_est, se) |>
    mutate(wt = 1 / sqrt(se)) |>
    select(-se) |>
    rename(index = log_est) |>
    mutate(index = round(index, decimals),
           wt = round(wt, decimals))

  lst <- NULL
  lst[[1]] <- j[, 1:2]
  lst[[2]] <- enframe(rep(gear_num, nrow(j)), name = NULL)
  lst[[3]] <- enframe(rep(area, nrow(j)), name = NULL)
  lst[[4]] <- enframe(rep(group, nrow(j)), name = NULL)
  lst[[5]] <- enframe(rep(sex, nrow(j)), name = NULL)
  lst[[6]] <- j[, ncol(j)]
  lst[[7]] <- enframe(rep(survey_timing, nrow(j)), name = NULL)

  k <- do.call("cbind", lst)
  names(k) <- c("year", "index", "gear", "area", "group", "sex", "wt", "survey_timing")

  write.table(k, file = out_fn, append = FALSE, quote = FALSE, row.names = FALSE,
              col.names = TRUE)
  message("Created file ", out_fn)
}