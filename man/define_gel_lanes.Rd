% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/define_gel_lanes.R
\name{define_gel_lanes}
\alias{define_gel_lanes}
\title{Define Gel Lanes}
\usage{
define_gel_lanes(gel_img, x_inflect_threshold = 25, expand_lanes = 10,
  plot_signal = T)
}
\arguments{
\item{gel_img}{an image object, created using the imager package.}

\item{expand_lanes}{how far in each direction from the midpoint should lanes be "expanded? 10 by default.}

\item{plot_signal}{Display the locations of the ranges? Useful for ensuring accuracy.}

\item{inflect_threshold}{px threshold for calculating minima; integer, 25 by default.}
}
\value{
Returns a dataframe of the calculated information.  Can also return a plot.
}
\description{
This function uses the inflect function (bioKIT) to identify local minima in an agarose gel image.  From this set of minima, the middle of each lane can be defined.  This returns a range, expanded by a set of pixels on either side of the middle.
}
\examples{
# ADD_EXAMPLES_HERE
}
