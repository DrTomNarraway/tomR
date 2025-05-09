% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_empty.R
\name{is_empty}
\alias{is_empty}
\title{Is Empty?}
\usage{
is_empty(object)
}
\arguments{
\item{object}{Any R object.}
}
\value{
If an object of length 1 is passed: returns true if the object is False, "", " ", NULL, NaN, or Inf; and returns false otherwise.
If the passed object has length 2+: returns a vector of the same length with T/F for each element of the object.
}
\description{
Check if the passed object is "empty".
}
