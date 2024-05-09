#' arrowtooth package
#'
#' See the README on
#' \href{https://github.com/pbs-assess/arrowtooth#readme}{GitHub}
#'
#' @docType package
#' @name arrowtooth

#' @import ggplot2
#' @importFrom hake plot_map
#' @importFrom rlang .data
#' @importFrom sf st_as_sf st_cast st_combine st_crs st_set_crs st_transform
#' @importFrom stats quantile
#' @importFrom utils capture.output
#'
#'
NULL

# from: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
# quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables(c(".", "total", "usability_code", "ageing_method", "is_mature",
                         "log_est", "wt"))
