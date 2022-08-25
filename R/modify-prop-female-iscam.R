#' Modifies an iSCAM data file to use the proportion female supplied
#'
#' @param fns The file name (*.dat) file to modify
#' @param prop The proportion (equal or less than 1) of females to insert
#'
#' @return Nothing
#' @export
modify_prop_female_iscam <- function(fn, prop = 0.79){

  if(is.null(prop) || prop < 0 || prop > 1){
    stop("`prop` must be a value between 0 and 1", call. = FALSE)
  }

  if(file.exists(fn)){
    d <- readLines(fn)
    prop_line <- grep("proportion population female", d)
    if(!length(prop_line)){
      stop("The proportion line was not found in file `", fn, "`, file not modified.",
           call. = FALSE)
    }
    d[prop_line] <- paste0(prop, "     # -proportion population female (propfem) Only used if nsex == 2")
    writeLines(d, fn)
    message("File `", fn, "` was modified with a female proportion of ", prop, ".")
  }else{
    message("File `", fn, "` does not exist. Nothing was modified.",
            call. = FALSE)
  }
}

mod_all_prop_fem_in_path <- function(path, ...){
  all_dirs <- list.dirs(path)

  walk(all_dirs, function(dr, ...){
    fns <- list.files(dr, pattern = "arf.dat")
    if(length(fns)){
      # arf.dat exists
      fn <- file.path(dr, "arf.dat")
      modify_prop_female_iscam(fn, ...)
    }
  }, ...)
}