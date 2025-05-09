% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Z.R
\name{Z}
\alias{Z}
\title{Z (Standardize Variable)}
\usage{
Z(sample, dps = 0)
}
\arguments{
\item{sample}{numeric vector // The variable to standardize.}

\item{dps}{numeric, default = 3 // Value to round to. Pass in 0 or FALSE to not round.}
}
\value{
numeric vector // The standardized vector.
}
\description{
Standardize a variable to allow easy comparison with other standardized variables.
}
