% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc-sex-props.R
\name{convert_length_to_weight_by_sex}
\alias{convert_length_to_weight_by_sex}
\title{Convert all lengths to weights for each sex for the input data frame \code{dat}}
\usage{
convert_length_to_weight_by_sex(d, lw_params)
}
\arguments{
\item{d}{A data frame returned by running \code{\link[gfdata:get_data]{gfdata::get_survey_samples()}} or
\code{\link[gfdata:get_data]{gfdata::get_commercial_samples()}}}

\item{lw_params}{A two-element list of vectors of alpha, Beta with males first and females second
in the list}
}
\value{
A data frame with weights calculated from the LW relationship
}
\description{
Convert all lengths to weights for each sex for the input data frame \code{dat}
}
