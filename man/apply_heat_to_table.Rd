% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_heat_to_table.R
\name{apply_heat_to_table}
\alias{apply_heat_to_table}
\title{Apply Heat to Table}
\usage{
apply_heat_to_table(table)
}
\arguments{
\item{table}{The table to apply colouring to.}
}
\value{
A gridExtra::tableGrob containing the values from the matrix.
}
\description{
Heat up the tables of a gridExtra::tableGrob so that the largest
values glow red hot. In other words: convert it to a heatmap.
}
\note{
This function will only add shading to positive values, you should use
colour_blue_to_orange for tables with positive and negative values.
}
