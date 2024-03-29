% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc-sex-props.R
\name{est_lw_params}
\alias{est_lw_params}
\title{Estimate the length/weight parameters for the dataset \code{d}. Columns \code{weight}
and \code{length} are required}
\usage{
est_lw_params(d, start_a = 0.05, start_b = 2, maxiter = 500)
}
\arguments{
\item{d}{A data frame returned by running \code{\link[gfdata:get_data]{gfdata::get_survey_samples()}} or
\code{\link[gfdata:get_data]{gfdata::get_commercial_samples()}}}

\item{start_a}{Starting alpha value for LW estimation. See \code{\link[stats:nls]{stats::nls()}}.}

\item{start_b}{Starting beta value for LW estimation. See \code{\link[stats:nls]{stats::nls()}}.}

\item{maxiter}{Maximum iterations in estimation. See \code{\link[stats:nls]{stats::nls()}}.}
}
\value{
The estimated parameters alpha (a) and beta (b) for these data w = a l ^ b
}
\description{
Estimate the length/weight parameters for the dataset \code{d}. Columns \code{weight}
and \code{length} are required
}
