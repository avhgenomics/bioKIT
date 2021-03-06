% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepTools2_wrapper.R
\name{dT2_plotProfile}
\alias{dT2_plotProfile}
\title{dT2 plot Profile}
\usage{
dT2_plotProfile(gz_file, output_file, dpi = 600, run_as_sys = F,
  outFileSortedRegions = NULL, outFileNameData = NULL, perGroup = F)
}
\arguments{
\item{gz_file}{gz file from dT2_computeMatrix}

\item{output_file}{output file (.png, jpg, etc)}

\item{dpi}{Resolution of the image}

\item{run_as_sys}{Run the command as system.}

\item{outFileSortedRegions}{path to output bedfile for sorted regions}

\item{outFileNameData}{path to output tab file for underlying data.}

\item{perGroup}{calculate profiles as per group, rather than per sample.}
}
\value{
returns a command string, or runs the command and returns an image.
}
\description{
Wrapper for deepTools2 plotProfile function.
}
\examples{
# ADD_EXAMPLES_HERE
}
