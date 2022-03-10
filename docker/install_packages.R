install.packages(c("devtools", "remotes", "TMB"))

# For index.Rmd (doc)
install.packages("tidylog")
# For twg_01.Rmd:
install.packages(c("cowplot", "showtext"))

# Make sure INLA download doesn't time out
options(timeout = 500)
# INLA install command from https://github.com/pbs-assess/gfplot
install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)

remotes::install_github("pbs-assess/sdmTMB")

# Install knitr and xaringan presentation package
remotes::install_github("yihui/xfun")
remotes::install_github("yihui/knitr")
remotes::install_github("yihui/xaringan")
remotes::install_github("gadenbuie/xaringanthemer")

# gifski is used to create GIF animations in presentations
remotes::install_github("r-rust/gifski")

# Install main package and its dependencies
remotes::install_github("pbs-assess/gfplot")
remotes::install_github("pbs-assess/arrowtooth")
