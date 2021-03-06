% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concentrate_cells.R
\name{concentrate_cells}
\alias{concentrate_cells}
\title{Concentrate Cells}
\usage{
concentrate_cells(total_cell_number, desired_conc_per_ul)
}
\arguments{
\item{total_cell_number}{The total number of cells in a pellet or sample.}

\item{desired_conc_per_ul}{the density of cells per ul desired. ex: 10000 (10000 / ul).  Also, can be used creatively: 10000/20 would be 10000 cells per 20 ul.}
}
\value{
Returns a string that contains the volume required in ul.
}
\description{
concentrate_cells() is a simple function to estimate the volume of buffer required to resuspend cells at a certain density / ul.
}
\examples{
# concentrate_cells(total_cell_number = 1000000,desired_conc_per_ul = 1000/5)
}
