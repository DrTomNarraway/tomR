% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bis.R
\name{bis}
\alias{bis}
\title{Balanced integration score}
\usage{
bis(rt, pc)
}
\arguments{
\item{rt}{Numeric vector of response times.}

\item{pc}{Numeric vector of proportion correct scores.}
}
\value{
A vector of scores of the same length as the passed vectors.
}
\description{
Calculate BIS for a given block. Each number should represent one
Participants average across trials. BIS is able to integrate speed and accuracy
much more fairly than most other integrated measures because we first standardize
the distributions and then simply subtract one from the other. Here we subtract
RT from PC which is something akin to a measure of ability (above or below average).
BIS (of this form) can be formally expressed as,
\deqn{BIS = Z_{pc} - Z_{rt}}
}
\note{
In order to Z standardize a value it is necessary to divide by the standard
deviation of that values population. We run into a problem if there are no errors in a given block,
as this means there is no variance and thus the standard deviation equals 0.
When this is the case we use the alternate formula as follows,
\deqn{BIS = -1 \cdot Z_{rt}}
}
