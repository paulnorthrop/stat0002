% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{sea_temps}
\alias{sea_temps}
\title{Southern Hemisphere temperature anomalies 1850-2022}
\format{
A dataframe with 173 rows and 2 columns:
\itemize{
\item{\code{year}:}{ the year.}
\item{\code{temp}:}{ the temperature anomaly in degrees C.}
}
}
\source{
Kennedy, J. J., Rayner, N. A., Atkinson, C. P., & Killick, R. E.
  (2019). An ensemble data set of sea surface temperature change from 1850:
  The Met Office Hadley Centre HadSST.4.0.0.0 data set.
  \emph{Journal of Geophysical Research: Atmospheres}, \strong{124},
  7719 - 7763. \doi{10.1029/2018JD029867}.
}
\usage{
sea_temps
}
\description{
Annual average sea surface temperature anomalies (from the 1961-1990
average) for the Southern Hemisphere for 1850-2022.
}
\examples{
# Extract data from 1946-2022
s46 <- sea_temps[sea_temps$year> 1945, ]
plot(s46, ylab = "temperature difference (degrees C)")
}
\keyword{datasets}
