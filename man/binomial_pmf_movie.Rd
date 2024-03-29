% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binomial_pmf_movie.R
\name{binomial_pmf_movie}
\alias{binomial_pmf_movie}
\title{Binomial p.m.f. movie}
\usage{
binomial_pmf_movie(
  starting_n = 1,
  starting_p = 1/2,
  delta_n = 1,
  delta_p = 0.05,
  observed_value = NA
)
}
\arguments{
\item{starting_n}{A numeric scalar.  The value of n for the first graph.}

\item{starting_p}{A numeric scalar.  The value of p for the first graph.}

\item{delta_n}{A numeric scalar.  The amount by which n is increased
(or decreased) after one click of the + (or -) button in the parameter
window.}

\item{delta_p}{A numeric scalar.  The amount by which p is increased
(or decreased) after one click of the + (or -) button in the parameter
window.}

\item{observed_value}{A non-negative integer.  If \code{observed_value} is
supplied then the corresponding line in the plot of the p.m.f. is coloured
in red.  If \code{observed_value} is not an integer then
\code{round(observed_value)} is used.}
}
\value{
Nothing is returned, only the animation is produced.
}
\description{
A movie to illustrate how the probability mass function (p.m.f.) of a
binomial (n, p) random variable depends on n and p. For more discrete
distributions, including the geometric and Poisson distributions, use
\code{smovie::movies()} and click on the \strong{Discrete} menu.
This is based on the \code{\link[smovie]{discrete}} function.
(If you have not installed the \code{smovie} package then use
\code{install.packages("smovie")} to install it.)
}
\details{
The probability mass function of a binomial random variable with
  parameters \eqn{n} (the number of Bernoulli trials performed) and
  \eqn{p} (the probabilities of success on a each trial) is plotted.
  The values of \eqn{n} and \eqn{p} can be changed by clicking on the
  relevant buttons.
}
\examples{
binomial_pmf_movie()

# Increase n and see what happens
binomial_pmf_movie(delta_n = 10)

# Sample size of the Aussie births data (26 boys, 18 girls)
binomial_pmf_movie(starting_n = 44, starting_p = 0.1, delta_p = 0.1,
                   observed_value = 26)

# Start at p = 0.591 (approximately 26/44)
binomial_pmf_movie(starting_n = 44, starting_p = 0.591, delta_p = 0.01,
                   observed_value = 26)
}
\seealso{
\code{\link{stat0002movies}}: general information about the movies.
}
