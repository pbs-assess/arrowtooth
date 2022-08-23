#' Return the location of the bash scripts for this package on your machine
#'
#' @return The full path where the bash scripts are located
#' @export
get_bash_path <- function(){
  file.path(system.file(package = "arrowtooth"), "bash-scripts")
}

#' Call [shell()] or [system()] depending on the Operating System
#'
#' @param ... Pass all arguments to the command function
#'
#' @return The output from the command function called
#' @export
system_ <- function(...){
  if(get_os() == "windows"){
    shell(...)
  }else{
    system(...)
  }
}

#' Return the Operating System name in lower case
#'
#' @return A character string describing the OS, e.g. 'windows', 'linux', or 'osx'
#' @export
get_os <- function(){
  sysinf <- Sys.info()
  if(!is.null(sysinf)){
    os <- sysinf["sysname"]
    if(os == "Darwin")
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}