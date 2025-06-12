#' arrowtooth package
#'
#' See the README on
#' \href{https://github.com/pbs-assess/arrowtooth#readme}{GitHub}
#'
#' @docType _PACKAGE
#' @name arrowtooth
#'
#' @import gfplot
#' @import ggplot2
#' @import lubridate
#' @import purrr
#' @import sdmTMB
#'
#' @importFrom PBSmapping clipPolys
#' @importFrom rlang .data global_env
#' @importFrom scales comma
#' @importFrom sdmTMB add_utm_columns delta_gamma get_index make_mesh
#' @importFrom sdmTMB pc_matern sanity sdmTMB sdmTMBcontrol sdmTMBpriors tweedie
#' @importFrom sf st_as_sf st_cast st_combine st_crs st_point st_sfc st_set_crs
#' @importFrom sf st_transform
#' @importFrom stats quantile
#' @importFrom stringr str_to_title
#' @importFrom TMB openmp
#' @importFrom utils capture.output

NULL

# from: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
# quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables(c(".", "total", "usability_code", "ageing_method", "is_mature",
                         "log_est", "wt"))
