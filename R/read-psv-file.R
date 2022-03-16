#' Read in a binary PSV file from an ADMB mcmc output run
#'
#' @param fn The filename
#' @param num_records The number of records to extract from the file.
#' See [readBin()] for more details
#'
#' @return A matrix with the parameters as coulmns and the records as rows
#' @export
read_psv_file <- function(fn, num_records = 1000){
  psv <- file(fn, "rb")
  num_params <- readBin(psv, "integer", n = 1)
  file_contents <- matrix(readBin(con = psv,
                                  what = "numeric",
                                  n = num_params * num_records),
                                  ncol = num_params,
                                  byrow = TRUE)
  close(psv)
  file_contents
}