#' The chain length of the Monte Carlo Markov Chain (MCMC)
#'
#' @format A single numeric value
"mcmc_chain_length"

#' Save the output every nth step of the MCMC chain.
#' If this is 1, save every step, if 2, every second step, etc.
#'
#' @format A single numeric value
"mcsave"

#' Rescale step size for this many initial steps in the MCMC
#'
#' @format A single numeric value
"mcscale"

#' The maximum number of function evaluations to allow in the MCMC
#'
#' @format A single numeric value
"maxfn"

#' Set gradient magnitude convergence criterion to this value
#'
#' @format A single numeric value
"crit"

#' Don’t show vector and gradient values in function minimizer screen report
#'
#' @format A logical. TRUE of FALSE
"nox"

#' The number of steps at the beginning of the chain to burn-in (discard)
#'
#' @format A single numeric value
"burnin"

#' The mcmc_chain_length divded by the mcsave value
#'
#' @format A single numeric value
"tot_num_posts"

#' The tot_num_posts minus the burnin value. This is the actual number
#' of chain steps used in the assessment
#'
#' @format A single numeric value
"num_posts_used"

#' Species common name
#'
#' @format A character string
"sp"

#' Species science name
#'
#' @format A character string
"sp_science"

#' Species colloquial name
#'
#' @format A character string
'sp_colloq'

#' Species family name
#'
#' @format A character string
'sp_family'

#' Short for British Columbia
#'
#' @format A character string
"bc"

#' Short for West Coast Vancouver Island
#'
#' @format A character string
"wcvi"

#' Short for Queen Chalotte Sound
#'
#' @format A character string
"qcs"

#' Short for Hecate Strait
#'
#' @format A character string
"hs"

#' Short for West Coast Vancouver Island Synoptic Survey
#'
#' @format A character string
"wcviss"

#' Short for Queen Charlotte Sound Synoptic Survey
#'
#' @format A character string
"qcsss"

#' Short for Hecate Strait Synoptic Survey
#'
#' @format A character string
"hsss"

#' Short for Hecate Strait Multispecies Assemblage Survey
#'
#' @format A character string
"hsmas"

#' Assessment year
#'
#' @format A character string
"assess_yr"

#' The last year the stock was assessed
#'
#' @format A character string
"last_assess_yr"

#' Catch data starts in this year for this assessment
#'
#' @format A character string
"start_catch_yr"

#' Catch data ends in this year for this assessment
#'
#' @format A character string
"end_catch_yr"

#' All ages older than this will be lumped into this age, caled a plus group
#'
#' @format A character string
"age_plus"


