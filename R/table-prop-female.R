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
#' @param bold_headers If `TRUE`, make all column headers bold
#'
#' @return A [csasdown::csas_table()]
#' @importFrom csasdown csas_table
#' @importFrom kableExtra row_spec
#' @export
table_prop_female <- function(prop_lst,
                              end_yr = 2019,
                              return_df = FALSE,
                              yrs = NULL,
                              ret_means = FALSE,
                              bold_header = TRUE,
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

  means <- vec2df(c(tr("Mean"), f(map_dbl(d[-1], ~{mean(.x, na.rm = TRUE)}), 2)),
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

  if(return_df){
    return(x)
  }

  # Translate the Year column header only
  names(x) <- gsub("Year", tr("Year"), names(x))

  out <- csas_table(x,
                    format = "latex",
                    booktabs = TRUE,
                    linesep = "",
                    bold_header = bold_header,
                    ...)
  if(!is.null(attr(out, "format"))){
    out <- out |>
      row_spec(nrow(x) - 1, hline_after = TRUE) |>
      row_spec(nrow(x), bold = TRUE)
  }
  out
}
