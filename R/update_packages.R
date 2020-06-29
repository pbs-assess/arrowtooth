update_packages <- function(){
  git_dir <- "C:/github"
  if(tolower(Sys.getenv("USERNAME")) == "grandin"){
    git_dir <- "C:/github/pbs-assess"
  }
  curr_dir <- getwd()

  cat(crayon::green("\nRebasing new commits from gfdlm...\n"))
  shell(paste0("cd ", file.path(git_dir, "gfdlm"), " && git pull --rebase"))
  cat(crayon::green("\nRebasing new commits from rosettafish...\n"))
  shell(paste0("cd ", file.path(git_dir, "rosettafish"), " && git pull --rebase"))
  cat(crayon::green("\nRebasing new commits from gfutilities...\n"))
  shell(paste0("cd ", file.path(git_dir, "gfutilities"), " && git pull --rebase"))
  cat(crayon::green("\nRebasing new commits from gfiscamutils...\n"))
  shell(paste0("cd ", file.path(git_dir, "gfiscamutils"), " && git pull --rebase"))
  cat(crayon::green("\nRebasing new commits from csasdown...\n"))
  shell(paste0("cd ", file.path(git_dir, "csasdown"), " && git pull --rebase"))

  setwd(git_dir)
  cat(crayon::green("\nBuilding and installing gfdlm package...\n"))
  devtools::install("gfdlm", quick = TRUE, dependencies = FALSE)
  cat(crayon::green("\nBuilding and installing rosettafish package...\n"))
  devtools::install("rosettafish", quick = TRUE, dependencies = FALSE)
  cat(crayon::green("\nBuilding and installing gfutilities package...\n"))
  devtools::install("gfutilities", quick = TRUE, dependencies = FALSE)
  cat(crayon::green("\nBuilding and installing gfiscamutils package...\n"))
  devtools::install("gfiscamutils", quick = TRUE, dependencies = FALSE)
  cat(crayon::green("\nBuilding and installing csasdown package...\n"))
  devtools::install("csasdown", quick = TRUE, dependencies = FALSE)
  setwd(curr_dir)
}