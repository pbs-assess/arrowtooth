#' Create files with Freezer trawler and Shoreside catch in the format required
#' for direct pasting into the iSCAM data file
#'
#' @param ct Output from [gfdata::get_catch()]
#' @param areas Area list as required by [gfplot::tidy_catch()]
#' @param years Years to include in the output
#' @param divisor A value to divide all catch values by
#' @param digits Number of significant figures to output
#' @param write_files Logical. If `TRUE`, write the catch tables to files. If
#' `FALSE`, return a list of two data frames
#' @param single_fleet Logical. If `TRUE`, the catch will be for one fleet,
#' which contains the sum of all catch for both fleets
#' @param fns A vector of two filenames
#' @param middle_text A vector of middle column data to insert in the output
#' @param ... Arguments passed to [gfplot::tidy_catch()] and in turn, to
#' [gfplot::set_fishing_year()]
#'
#' @return Nothing is `write_files` is `TRUE`. A list of two data frames if
#' `write_files` is `FALSE`
#' @importFrom lubridate month day
#' @importFrom gfiscamutils vec2df modify_model_path
#' @importFrom gfplot tidy_catch fit_vb
#' @importFrom dplyr bind_cols
#' @importFrom here here
#' @importFrom utils write.table
#' @importFrom tibble enframe
#' @export
export_catch <- function(ct,
                         areas = c("3[CD]+", "5[ABCDE]+"),
                         years = 1996:2021,
                         divisor = 1e6,
                         digits = 4,
                         write_files = TRUE,
                         single_fleet = FALSE,
                         fns = c(paste0("catch-ft-", Sys.Date(), ".txt"),
                                 paste0("catch-ss-", Sys.Date(), ".txt")),
                         middle_text = c(gear = 1,
                                         group = 1,
                                         area = 1,
                                         sex = 0,
                                         type = 1),
                         ...){

  options(pillar.sigfig = 7)

  if(single_fleet){
    catch_all <- ct |>
      tidy_catch(areas = areas, ...) |>
      group_by(year) |>
      summarize(round(sum(value / divisor), digits)) |>
      filter(year %in% years) |>
      complete(year = years) |>
      rename(catch = 2)
    catch_all[is.na(catch_all)] <- 0
    if(!is.null(middle_text)){
      middle_df <- vec2df(middle_text, nms = names(middle_text))
      middle_df <- do.call("rbind",
                           replicate(nrow(catch_all),
                                     middle_df,
                                     simplify = FALSE))
      catch_all <- bind_cols(catch_all[1], middle_df, catch_all[2])
    }

    if(write_files){
      nongit_dir <- file.path(dirname(here()), "arrowtooth-nongit")
      dir.create(file.path(nongit_dir, "data-output"), showWarnings = FALSE)
      fn <- file.path(nongit_dir,
                      file.path("data-output",
                                fns[1]))

      write.table(catch_all, fn,
                  quote = FALSE,
                  row.names = FALSE)
      message("Created file ", fn)
      return(invisible())
    }else{
      return(list(catch_all))
    }
  }

  # Freezer trawler catch -----------------------------------------------------
  catch_ft <-
    extract_fleet_catch(ct) |>
    tidy_catch(areas = areas, ...) |>
    group_by(year) |>
    summarize(round(sum(value / divisor), digits)) |>
    filter(year %in% years) |>
    complete(year = years) |>
    rename(catch = 2)
  catch_ft[is.na(catch_ft)] <- 0
  if(!is.null(middle_text)){
    middle_df <- vec2df(middle_text, nms = names(middle_text))
    middle_df <- do.call("rbind",
                         replicate(nrow(catch_ft),
                                   middle_df,
                                   simplify = FALSE))
    catch_ft <- bind_cols(catch_ft[1], middle_df, catch_ft[2])
  }

  # Shoreside catch -----------------------------------------------------------
  catch_ss <- extract_fleet_catch(ct, include = FALSE) |>
    tidy_catch(areas = areas, ...) |>
    group_by(year) |>
    filter(year %in% years) |>
    summarize(round(sum(value / divisor), digits)) |>
    complete(year = years) |>
    rename(catch = 2)
  catch_ss[is.na(catch_ss)] <- 0
  if(!is.null(middle_text)){
    middle_df <- vec2df(middle_text, nms = names(middle_text))
    middle_df <- do.call("rbind",
                         replicate(nrow(catch_ss),
                                   middle_df,
                                   simplify = FALSE))
    catch_ss <- bind_cols(catch_ss[1], middle_df, catch_ss[2])
  }

  if(write_files){
    nongit_dir <- file.path(dirname(here()), "arrowtooth-nongit")
    dir.create(file.path(nongit_dir, "data-output"), showWarnings = FALSE)
    fns[1] <- file.path(nongit_dir,
                        file.path("data-output",
                                  fns[1]))
    fns[2] <- file.path(nongit_dir,
                        file.path("data-output",
                                  fns[2]))
    write.table(catch_ft, fns[1],
                quote = FALSE,
                row.names = FALSE)
    message("Created file ", fns[1])
    write.table(catch_ss, fns[2],
                quote = FALSE,
                row.names = FALSE)
    message("Created file ", fns[2])
  }else{
    list(catch_ft, catch_ss)
  }
}