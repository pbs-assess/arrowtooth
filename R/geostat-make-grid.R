#' Create a grid data frame for the Geo statistical analyses
#'
#' @param x A data frame with location
#' @param years The years to append to replicate rows in the grid data frame
#'
#' @return A data frame
#' @export
make_grid <- \(x, years) {

  years <- sort(unique(years))
  nd <- replicate(length(years),
                  x,
                  simplify = FALSE) |>
    bind_rows()
  nd$year <- rep(years, each = nrow(x))

  nd
}