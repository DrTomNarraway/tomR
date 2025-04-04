% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d_prime.R
\name{d_prime}
\alias{d_prime}
\title{Calculate d' (d prime).}
\usage{
d_prime(h, fa, n)
}
\arguments{
\item{h}{Number or numeric-vector of hits.}

\item{fa}{Number  or numeric-vector of false alarms.}

\item{n}{Number  or numeric-vector of total trials, i.e. hits, false alarms, and misses.}
}
\value{
A number or numeric-vector of d' values.
}
\description{
Calculate the signal detection theory value d'.
This function automatically prevents H = 1 and FA = 0 errors by limiting h and fa based on n.
}
