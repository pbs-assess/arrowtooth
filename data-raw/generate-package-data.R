# If you add a package variable here, you need to add it further down as well within a `use_data()`
# command, and include a descriptor chunk in data.R. Source this file, then run
# `devtools::document()` and `devtools::build()`

mcmc_chain_length <- 2000
mcsave <- 1
mcscale <- 0
maxfn <- 2000
crit <- 0.0001
nox <- TRUE

burnin <- 1000
tot_num_posts <- mcmc_chain_length / mcsave
num_posts_used <- tot_num_posts - burnin

sp <- "Arrowtooth Flounder"
sp_science <- "Atheresthes stomias"
sp_colloq <- "Turbot"
sp_family <- "Pleuronectidae"
bc <- "British Columbia"
wcvi <- "West Coast Vancouver Island"
qcs <- "Queen Charlotte Sound"
hs <- "Hecate Strait"
wcviss <- paste(wcvi, "Synoptic Survey")
qcsss <- paste(qcs, "Synoptic Survey")
hsss <- paste(hs, "Synoptic Survey")
hsmas <- paste(hs, "Multispecies Assemblage Survey")

assess_yr <- 2020
last_assess_yr <- 2015
start_catch_yr <- 1954
end_catch_yr <- assess_yr
age_plus <- 20

use_data(mcmc_chain_length, overwrite = TRUE)
use_data(mcsave, overwrite = TRUE)
use_data(mcscale, overwrite = TRUE)
use_data(maxfn, overwrite = TRUE)
use_data(crit, overwrite = TRUE)
use_data(nox, overwrite = TRUE)
use_data(burnin, overwrite = TRUE)
use_data(tot_num_posts, overwrite = TRUE)
use_data(num_posts_used, overwrite = TRUE)
use_data(sp, overwrite = TRUE)
use_data(sp_science, overwrite = TRUE)
use_data(sp_colloq, overwrite = TRUE)
use_data(sp_family, overwrite = TRUE)
use_data(bc, overwrite = TRUE)
use_data(wcvi, overwrite = TRUE)
use_data(qcs, overwrite = TRUE)
use_data(hs, overwrite = TRUE)
use_data(wcviss, overwrite = TRUE)
use_data(qcsss, overwrite = TRUE)
use_data(hsss, overwrite = TRUE)
use_data(hsmas, overwrite = TRUE)
use_data(assess_yr, overwrite = TRUE)
use_data(last_assess_yr, overwrite = TRUE)
use_data(start_catch_yr, overwrite = TRUE)
use_data(end_catch_yr, overwrite = TRUE)
use_data(age_plus, overwrite = TRUE)
