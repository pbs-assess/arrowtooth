#' Create a table of weights used in the proportion female analysis
#'
#' @param samples Output from either [gfdata::get_commercial_samples()] or
#' [gfdata::get_survey_samples()] depending on the value of `type`
#' @param type Which type to return, "commercial" or "survey"
#' @param ord A vector of survey names as they appear in the output data frame,
#' in the order you want them in the output. If `NULL`, order will be ignored
#' @param return_df If `TRUE`, return a data .frame instead of a
#' @param col_widths Widths for columns, except the Parameter column
#' the [csasdown::csas_table()]
#' @param end_yr The last year to include in the table
#' [csasdown::csas_table()]
#' @param bold_headers If `TRUE`, make all column headers bold
#' @param ... Arguments passed to [csasdown::csas_table()]
#'
#' @return A [csasdown::csas_table()]
#' @export
table_prop_female_weights <- function(samples = NULL,
                                      type = c("commercial", "survey"),
                                      end_yr = 2019,
                                      ord = c("QCS", "HS", "WCVI", "WCHG"),
                                      return_df = FALSE,
                                      col_widths = NULL,
                                      bold_header = TRUE,
                                      ...){

  type <- match.arg(type)
  if(is.null(samples)){
    if(type == "commercial"){
      stop("`samples` is `NULL`. It must be output of ",
           "gfdata::get_commercial_samples()", call. = FALSE)
    }else{
      stop("`samples` is `NULL`. It must be output of ",
           "gfdata::get_survey_samples()", call. = FALSE)
    }
  }
  if(type == "commercial"){
    d <- props_comm_data_summary(samples)
  }else{
    d <- props_surv_data_summary(samples)
  }

  d <- d |>
    filter(d$Year <= end_yr)

  if(type == "survey"){
    # HACK
    d <- d |>
      mutate(Survey = ifelse(is.na(Survey), "HS", Survey))
  }

  if(type == "survey" && !is.null(ord)){
    d <- imap(ord, ~{
      j <- d |>
        filter(Survey == .x)
    }) |>
      map_df(~{.x})
  }

  if(return_df){
    return(d)
  }
  # Translate column names
  survey_sym <- sym(tr("Survey"))
  year_sym <- sym(tr("Year"))
  num_trips_sym <- sym(tr("Number of trips"))
  num_samples_sym <- sym(ifelse(fr(),
                                "Nombre d'échant-illons",
                                "Number of samples"))
  num_weights_m_sym <- sym(tr("Number of weights - Male"))
  num_weights_f_sym <- sym(tr("Number of weights - Female"))

  if(type == "survey"){
    d <- d |>
      mutate(Survey = tr(Survey)) |>
      rename(!!survey_sym := `Survey`,
             !!year_sym := `Year`,
             !!num_samples_sym := `Number of samples`,
             !!num_weights_m_sym := `Number of weights - Male`,
             !!num_weights_f_sym := `Number of weights - Female`)
  }else{
    d <- d |>
      rename(!!year_sym := `Year`,
             !!num_trips_sym := `Number of trips`,
             !!num_samples_sym := `Number of samples`,
             !!num_weights_m_sym := `Number of weights - Male`,
             !!num_weights_f_sym := `Number of weights - Female`)
  }

  names(d) <- names(d) |>
    str_wrap(16, whitespace_only = FALSE) |>
    linebreak()

  tab <- csas_table(d,
                    format = "latex",
                    booktabs = TRUE,
                    linesep = "",
                    bold_header = bold_header,
                    ...)

  if(type == "survey"){
    survs <- unique(d$Survey)
    wch <- map(survs, ~{which(d$Survey == .x)})
    end_rows <- map_dbl(wch, ~{tail(.x, 1)})
    end_rows <- end_rows[-length(end_rows)]
    tab <- tab |>
      row_spec(end_rows, hline_after = TRUE)
  }

  if(!is.null(col_widths)){
    tab <- tab |>
      column_spec(2:ncol(d), width = col_widths)
  }

  tab
}
