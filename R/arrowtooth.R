#' arrowtooth package
#'
#' See the README on
#' \href{https://github.com/pbs-assess/arrowtooth#readme}{GitHub}
#'
#' @docType package
#' @name arrowtooth

#' @import gfplot
#' @import ggplot2
#' @importFrom PBSmapping clipPolys
#' @importFrom rlang .data
#' @importFrom scales comma
#' @importFrom sf st_as_sf st_cast st_combine st_crs st_point st_sfc st_set_crs
#' @importFrom sf st_transform
#' @importFrom stats quantile
#' @importFrom utils capture.output
#'
#'
NULL

# from: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
# quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables(c(".", "total", "usability_code", "ageing_method", "is_mature",
                         "log_est", "wt"))
