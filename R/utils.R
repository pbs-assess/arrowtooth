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

#' Rotate the coordinates in a data frame
#'
#' @param df An [sf] data frame
#' @param rotation_angle The angle to rotate the coordinates
#' @param rotation_center A vector of 2: X and Y location for the
#' center to rotate about
#'
#' @return The modified [sf] data frame
#' @export
rotate_df <- function(df, rotation_angle, rotation_center){

  r <- rotate_coords(df$X,
                     df$Y,
                     rotation_angle = rotation_angle,
                     rotation_center = rotation_center
  )
  df$X <- r$x
  df$Y <- r$y

  df
}

#' Rotate vectors of coordinates
#'
#' @param x The X position vector
#' @param y The Y position vector
#' @param rotation_angle The angle to rotate the coordinates
#' @param rotation_center A vector of 2: X and Y location for the
#' center to rotate about
#'
#' @return A [tibble::tibble()] containing the rotated coordinates
#' @export
rotate_coords <- function(x, y, rotation_angle, rotation_center) {

  rot <- -rotation_angle * pi / 180
  newangles <- atan2(y - rotation_center[2], x - rotation_center[1]) + rot
  mags <- sqrt((x - rotation_center[1])^2 + (y - rotation_center[2])^2)
  x <- rotation_center[1] + cos(newangles) * mags
  y <- rotation_center[2] + sin(newangles) * mags

  tibble(x = x, y = y)
}
