#' Make a data frame from given GLMM model output showing the estimated
#' parameter values for each parameter
#'
#' @param object A model as returned by [run_discard_index_model()]
#'
#' @return A data frame representing GLMM model parameter value outputs
#' @export
make_re_dat <- function(object) {

  re <- glmmTMB::ranef(object)
  plyr::ldply(re$cond, \(x) {
    sud <- as.data.frame(x)
    sud$par_value <- row.names(sud)
    row.names(sud) <- NULL
    sud
  }) |>
    rename(par_group = .id) |>
    rename(est = `(Intercept)`) |>
    as_tibble() |>
    mutate(loc_group = gsub("^([0-9]+)[ -]*([0-9a-zA-Z-]+)$", "\\2", par_value)) |>
    mutate(loc_year = gsub("^([0-9]+)[ -]*[0-9a-zA-Z-]+$", "\\1", par_value))
}
