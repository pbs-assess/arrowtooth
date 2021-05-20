#' Rebase and build Groundfish packages automatically.
#'
#' @return Nothing
#' @export
update_gf_packages <- function(){
  git_dir <- "C:/github"
  if(tolower(Sys.getenv("USERNAME")) == "grandin"){
    git_dir <- "C:/github/pbs-assess"
  }
  orig_dir <- getwd()
  on.exit(setwd(orig_dir))

  # Git pull packages ---------------------------------------------------------
  cat(crayon::green("\nRebasing new commits from csasdown...\n"))
  shell(paste0("cd ", file.path(git_dir, "csasdown"), " && git pull --rebase"))

  cat(crayon::green("\nRebasing new commits from gfdata...\n"))
  shell(paste0("cd ", file.path(git_dir, "gfdata"), " && git pull --rebase"))

  cat(crayon::green("\nRebasing new commits from gfiscamutils...\n"))
  shell(paste0("cd ", file.path(git_dir, "gfiscamutils"), " && git pull --rebase"))

  cat(crayon::green("\nRebasing new commits from gfplot...\n"))
  shell(paste0("cd ", file.path(git_dir, "gfplot"), " && git pull --rebase"))

  cat(crayon::green("\nRebasing new commits from gfutilities...\n"))
  shell(paste0("cd ", file.path(git_dir, "gfutilities"), " && git pull --rebase"))

  cat(crayon::green("\nRebasing new commits from rosettafish...\n"))
  shell(paste0("cd ", file.path(git_dir, "rosettafish"), " && git pull --rebase"))

  # Install packages ----------------------------------------------------------
  setwd(git_dir)
  cat(crayon::green("\nBuilding and installing csasdown package...\n"))
  devtools::install("csasdown", quick = TRUE, dependencies = FALSE)

  cat(crayon::green("\nBuilding and installing gfdata package...\n"))
  devtools::install("gfdata", quick = TRUE, dependencies = FALSE)

  cat(crayon::green("\nBuilding and installing gfiscamutils package...\n"))
  devtools::install("gfiscamutils", quick = TRUE, dependencies = FALSE)

  cat(crayon::green("\nBuilding and installing gfplot package...\n"))
  devtools::install("gfplot", quick = TRUE, dependencies = FALSE)

  cat(crayon::green("\nBuilding and installing gfutilities package...\n"))
  devtools::install("gfutilities", quick = TRUE, dependencies = FALSE)

  cat(crayon::green("\nBuilding and installing rosettafish package...\n"))
  devtools::install("rosettafish", quick = TRUE, dependencies = FALSE)
}