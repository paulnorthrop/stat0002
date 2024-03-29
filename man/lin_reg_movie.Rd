% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simple_linear_regression_movie.R
\name{lin_reg_movie}
\alias{lin_reg_movie}
\title{Simple linear regression movie}
\usage{
lin_reg_movie(data, delta_alpha = 0.1, delta_beta = 1e-04, ...)
}
\arguments{
\item{data}{A data frame with two variables or a numeric matrix with
2 columns.  The first column must contain the response data, the second
column the explanatory data.}

\item{delta_alpha, delta_beta}{Numeric scalars.  The respective amounts by
which the values of the intercept and gradient of the line are
increased/decreased after one click of the +/- button.
The default values are set with the \code{\link{hubble}} data used
in the example below in mind.}

\item{...}{Further arguments, such as graphical parameters
(see \code{\link[graphics]{par}} to be passed to
\code{\link[graphics:plot.default]{plot}} when producing a scatter plot of the
response data against the explanatory data.  For example, the
plotting character used for the points can be chosen using \code{pch}.
If \code{pch} has length greater than 1 then only the first element
is used.  The default value of \code{pch} is 16 (filled circle).
The labels on the horizontal and vertical axes can be specified
using \code{xlab} and \code{ylab} respectively.}
}
\value{
Nothing is returned, only the animation is produced.
}
\description{
A movie to help to visualize the fitting of a regression line using
least squares in the case of a simple linear regression, that is,
a linear regression of one response variable on one explanatory variable.
}
\details{
A scatter plot of response data against the explanatory data
  is produced.  On this plot is superimposed a dashed line that the user
  can move, by changing its intercept alpha and gradient beta
  using +/- buttons.  The initial value of alpha is the mean of the
  response data and the initial value of beta is 0.
  The sizes of the residuals are shown using red lines.
  One of the legends gives the current sum of squares residuals (SS).

  Another +/- button allows the user to add the least squares regression
  line to the plot, and the associated residual sum of squares (RSS) to
  the legend, for comparison.
}
\examples{
# Produce movie using the hubble data
lin_reg_movie(hubble, pch = 16, xlab = "recession velocity (km/sec)",
              ylab = "distance (megaparsecs)")
}
\seealso{
\code{\link{stat0002movies}}: general information about the movies.
}
