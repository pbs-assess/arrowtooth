#' Create a table of proportions female
#'
#' @details
#' If `yrs` includes the last year, the mean row will also be included.
#' `yrs` is meant to split up the table for display purposes. The mean
#' is always calculated from ALL years, even if only some are returned.
#'
#' @param prop_lst A list of two outputs, the first from [props_comm()], and
#' the second from [props_surv()]
#' @param end_yr The last year to include in the table
#' @param return_df If `TRUE`, return a data .frame instead of a
#' [csasdown::csas_table()]
#' @param ... Arguments passed to [csasdown::csas_table()]
#' @param format The format of table as in [knitr::kable()]
#' @param yrs A vector of years to include in the output table. If `NULL`,
#' all years will be included
#' @param ret_means Logical. If `TRUE`, return a list of the gear mean values.
#' Takes priority over `ret_df`
#'
#' @return A [csasdown::csas_table()]
#' @importFrom csasdown csas_table
#' @importFrom kableExtra row_spec
#' @export
table_prop_female <- function(prop_lst,
                              end_yr = 2019,
                              return_df = FALSE,
                              format = "latex",
                              yrs = NULL,
                              ret_means = FALSE,
                              ...){

  ct_sym <- sym(tr("Commercial trawl"))
  qcs_sym <- sym(tr("QCS"))
  hs_sym <- sym(tr("HS"))
  wcvi_sym <- sym(tr("WCVI"))
  wchg_sym <- sym(tr("WCHG"))

  d <- map_df(prop_lst, ~{.x}) |>
    pivot_wider(names_from = "data_source", values_from = "prop_female") |>
    rename(Year = year,
           !!ct_sym := commercial_coastwide,
           !!qcs_sym := qcsss,
           !!hs_sym := hsss,
           !!wcvi_sym := wcviss,
           !!wchg_sym := wchgss)

  mean_vec <- d[-1] |> colMeans(na.rm = TRUE)
  if(ret_means){
    return(mean_vec)
  }

  j <- bind_cols(d[, 1], map_df(d[-1], ~{f(.x, 2)}))

  k <- bind_cols(j[, 1], map_df(j[-1], ~{gsub("\\s*NA\\s*", "--", .x)})) |>
    filter(Year <= end_yr) |>
    mutate(Year = as.character(Year))

  means <- vec2df(c("Mean", f(map_dbl(d[-1], ~{mean(.x, na.rm = TRUE)}), 2)),
                  nms = names(k))

  x <- bind_rows(k, means)

  if(!is.null(yrs)){
    final_year <- as.numeric(slice(x, nrow(x) - 1)$Year)
    if(final_year %in% yrs){
      yrs <- c(yrs, "Mean")
    }
    x <- x |>
        filter(Year %in% yrs)
  }
  if(format == "html"){
    x[x == "--"] <- "&mdash;"
  }

  if(return_df){
    return(x)
  }
  # Translate Year
  x <- x |>
    mutate(Year = tr("Year"))

  out <- csas_table(x,
             format = format,
             ...)
  if(!is.null(attr(out, "format"))){
    out <- out |>
      row_spec(nrow(x) - 1, hline_after = TRUE) |>
      row_spec(nrow(x), bold = TRUE)
  }
  out
}

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
  num_samples_sym <- sym(tr("Number of samples"))
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

  tab <- csas_table(d,
                    format = "latex",
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
