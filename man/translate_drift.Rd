% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formulae.R
\name{translate_drift}
\alias{translate_drift}
\title{Translate Drift Rate}
\usage{
translate_drift(d, ms_to_s = T, dps = 3, quiet = F)
}
\arguments{
\item{d}{numeric. The original drift rate to be converted.}

\item{ms_to_s}{bool, default = true. If true convert from ms to seconds.}

\item{dps}{numeric, default = 3. Value to round to. Pass in 0 or FALSE to not round.}

\item{quiet}{bool, default = false. If quiet do not print stuff to console.}
}
\description{
Translate drift rate with sigma = 4 in ms to sigma = 1 in seconds.
}
