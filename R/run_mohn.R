#' Create a data.frame from the retrospectives for input into Mohn's
#' calculations for retrospective patterns
#'
#' @param models A list of iscam model objects (class [mdl_lst_cls])
#'
#' @return A data.frame in the correct format to be input
#' into [icesAdvice::mohn()]
#' @export
format_mohn <- function(models, quant_name = "sbt_quants"){

  j <- imap(models$retro_grps[[1]], ~{
    as_tibble(.x$mcmccalcs[[quant_name]], rownames = "quants") |>
      filter(quants == "50%") |>
      t() |>
      as_tibble(rownames = "year") |>
      filter(year != "quants") |>
      rename(value = V1) |>
      mutate(value = as.numeric(value))

  })
  k <- j[[1]]
  for(i in 2:length(j)){
    k <- left_join(k, j[[i]], by = "year")
  }
  k <- as.data.frame(k)
  rownames(k) <- k$year
  k <- k[, -1]
  names(k) <- names(j)

  k
}