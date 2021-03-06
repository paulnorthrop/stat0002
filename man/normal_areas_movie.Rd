% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normal_areas_movie.R
\name{normal_areas_movie}
\alias{normal_areas_movie}
\title{Normal areas movie}
\usage{
normal_areas_movie(starting_multiple = 1, delta_multiple = 1, ndec = 3)
}
\arguments{
\item{starting_multiple}{A non-negative numeric scalar.  The value of the
multiple used in the first plot.}

\item{delta_multiple}{A numeric scalar.  The amount by which the value of
the multiple is increased/decreased after one click of the +/- button.}

\item{ndec}{The number of decimal places to which to round the probability
that is superimposed on the plot.}
}
\value{
Nothing is returned, only the animation is produced.
}
\description{
A movie to show how the probability that a standard normal random variable
lies within plus or minus a multiple of its standard deviation (that is, 1)
varies with the value of the multiple.
}
\details{
The value of the multiple can be changed using the
  +/i buttons in the panel.
  For the purposes of this movie, the multiple cannot be increased above
  a value of 5.
}
\examples{
normal_areas_movie()
}
\seealso{
\code{\link{stat0002movies}}: general information about the movies.

\code{\link{normal_pdf_movie}}: normal probability density function
  movie.
}
