#' Rebase and build Groundfish packages automatically
#' @param git_dir The directory where you have your pbs-assess git repositories
#' @param quick See `quick` argument in [devtools::install()]
#' @param dep See `dependencies` argument in [devtools::install()]
#' @return Nothing
#' @export
update_gf_packages <- function(git_dir = "C:/github",
                               quick = TRUE,
                               dep = FALSE){

  if(tolower(Sys.getenv("USERNAME")) == "chris-grandin"){
    git_dir <- "/home/chris-grandin/github/pbs-assess"
  }
  if(!dir.exists(git_dir)){
    stop("The directory `", git_dir, "` does not exist")
  }
  orig_dir <- getwd()
  on.exit(setwd(orig_dir))

  pkg_lst <- c("csasdown", "gfdata", "gfiscamutils",
               "gfplot", "gfutilities", "rosettafish")

  # Git pull packages ---------------------------------------------------------
  walk(pkg_lst, \(pkg){
    cat(crayon::green(paste0("\nRebasing new commits from ", pkg, "...\n")))
    system_(paste0("cd ", file.path(git_dir, pkg), " && git pull --rebase"))
  })

  # Install packages ----------------------------------------------------------
  setwd(git_dir)
  walk(pkg_lst, \(pkg){
    cat(crayon::green(paste0("\nBuilding and installing ", pkg, " package...\n")))
    devtools::install(pkg, quick = quick, dependencies = dep)
  })

}