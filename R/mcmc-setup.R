mcmc_chain_length <- 2000
mcsave <- 1
mcscale <- 0
maxfn <- 2000
crit <- 0.0001
nox <- TRUE

burnin <- 1000
tot_num_posts <- mcmc_chain_length / mcsave
num_posts_used <- tot_num_posts - burnin
