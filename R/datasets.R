#' Challenger Space Shuttle Disaster Dataset
#'
#' These data are discussed in the first STAT0002 lecture.
#' In a paper (Dalal \emph{et al.}, 1989) these data are analysed
#' with a view to estimating (retrospectively) the probability of a
#' catastrophic failure of the Challenger space shuttle under the
#' launch conditions on 28th January 1986.
#'
#' @format A dataframe with 24 rows and 5 columns:
#' \itemize{
#' \item{\code{flight}:}{ the flight number.}
#' \item{\code{date}:}{ the date.}
#' \item{\code{damaged}:}{ the number of O-rings with thermal distress.}
#' \item{\code{temperature}:}{ temperature, in degrees Fahrenheit.}
#' \item{\code{pressure}:}{ pressure, pounds per square inch.}}
#'
#' The first 23 rows contain data for test flights.  The last row
#' contains the data for the ill-fated flight on 28th January 1986,
#' where the number of damaged 0-rings is missing (NA).
#' @source Dalal, S. R, Fowlkes, E. B. and Hoadley, B. (1989)
#'   Risk Analysis of the Space Shuttle: Pre-Challenger Prediction
#'   of Failure. \emph{Journal of the American Statistical Association},
#'   \strong{84}(408), 945-957.
#'   \url{http://dx.doi.org/10.1080/01621459.1989.10478858}
#' @seealso \code{\link{shuttle_sim}}, \code{\link{shuttle_sim_plot}}
"shuttle"

#' Oxford Birth Times
#'
#' Times spent in the delivery suite by 95 women giving birth at the John
#' Radcliffe Hospital, Oxford. These data have been taken from the SMPracticals
#' package to which they were provided by Ethel Burns.
#'
#' @format A data frame with 95 observations on the following 2 variables.
#' \itemize{
#' \item{\code{day:}}{ Day on which the woman arrived.}
#' \item{\code{time:}}{ Time (hours) spent on delivery suite.}}
#'
#' @source Davison, A. C. (2003) Statistical Models. Cambridge University Press.
#'   Page 18.
"ox_births"

#' Mystery data in Exercises 1
#'
#' Your task in Exercises 1 is to guess what these data represent using only
#' summary plots and statistics.
#'
#' @format A numeric vector.  The unit of measurement is days.
"days"

#' T-cell count data
#'
#' The numbers of a certain type of T-cell (a white blood cell) per cubic mm
#' in blood samples taken from 42 patients in remission from from lymphoma.
#' 21 of the patients had Hodgkin's lymphoma, the other 21 had non-Hodgkin's
#' lymphoma.
#'
#' @format A data frame with 42 observations on the following 2 variables.
#' \itemize{
#' \item{\code{tcell:}}{ a numeric vector giving the number of T-cells.}
#' \item{\code{type:}}{ a factor giving the type of lymphoma: Hodgkin's or
#'   non-Hodgkin's.}}
"lymphoma"

#' Blood Types
#'
#' Percentages of people in the UK in the 8 main ABO-Rhesus blood groups.
#'
#' @format A data frame with 8 observations on the following 3 variables.
#' \itemize{
#' \item{\code{ABO:}}{ Blood type in the ABO system (A, B or O).}
#' \item{\code{rhesus:}}{ Blood type in the Rhesus system (Rh+ or Rh-).}
#' \item{\code{percentage:}}{ Percentage of people in the ABO-Rhesus blood type}.}
#'
"blood_types"

#' FTSE 100 Share Index
#'
#' Weekly closing prices of the FTSE 100 share index from 2nd April 1984 to
#' 13th August 2007.
#'
#' @format A data frame with 1220 observations on the following 2 variables.
#' \itemize{
#' \item{\code{date:}}{ the date.}
#' \item{\code{price:}}{ the closing price on that date.}}
#'
"ftse"

#' Influenza data
#'
#' The numbers of people (in thousands of people) in the UK visiting their
#' doctor with symptoms of influenza (flu) during four-weekly time periods
#' over the time period 28th January 1967 to 13th November 2004.
#'
#' @format A data frame with 494 observations on the following 2 variables.
#' \itemize{
#' \item{\code{date:}}{ the date.}
#' \item{\code{visits:}}{ the number of people visiting their doctor with
#' symptoms of flu.}}
#'
"flu"

#' The 2000 U.S. Presidential Election in Florida
#'
#' Voting results and demographic data for the state of Florida in the
#' United States presidential election in the year 2000.
#'
#' @format A data frame with 67 observations on 22 variables.
#' Each row relates to a county in Florida.
#' \itemize{
#' \item{Columns 1-4, county identifiers and location: }
#'   {county number \code{co}, county name \code{co_names},
#'   latitude in degrees north \code{lat},
#'   longitude in degrees west \code{lon}.}
#' \item{Columns 5-12, county demographic variables: }
#' {population in 1997 \code{npop},
#' percentage of whites in 1996 \code{whit},
#' percentage of blacks in 1996 \code{blac},
#' percentage of Hispanics in 1996 \code{hisp},
#' percentage of the population aged 65 and over in 1996 \code{o65},
#' percentage of the population that graduated from high school
#' (1990 census) \code{hsed},
#' percentage of the population that graduated from college
#' (1990 census) \code{coll},
#' mean personal income (1994) \code{inco}.}
#' \item{Columns 13-22, numbers of votes for candidates: }{
#' Bush \code{bush}, Gore \code{gore}, Browne \code{brow},
#' Nader \code{nade}, Harris \code{harr}, Hagelin \code{hage},
#' Buchanan \code{buch}, McReynolds \code{mcre}, Phillips \code{phil},
#' Moorehead \code{moor}}.
#' \item{Column 23, total number of votes cast: \code{tvot}}.}
#'
#' For full details see Tables 1 to 4 of
#' \href{http://dx.doi.org/10.1214/ss/1049993203}{Smith (2002)}.
#' @references Smith, R. L. (2002) A Statistical Assessment of Buchanan's
#'   Vote in Palm Beach County, \emph{Statistical Science}, \strong{17(4)},
#'   441-457, \url{http://dx.doi.org/10.1214/ss/1049993203}.
"USelection"

# ========================== World Bank data =========================

#' World Bank Data
#'
#' Selected annual World Development Indicators provided in the World Bank's
#' World Development Indicators database for the countries of the world,
#' 1960-2014.  The indicators are
#' \itemize{
#'   \item{\strong{co2_per_capita}: }{total carbon dioxide emissions per
#'     capita, in metric tons per person}
#'   \item{\strong{gdp_per_capita}: }{Gross Domestic Product (GDP) per
#'     capita, in current US dollars per person}
#'   \item{\strong{population_size}: }{total population size}
#'   \item{\strong{life_expectancy}: }{life expectancy at birth, in years}
#' }
#' @format A data frame with 217 rows (one for each country) and one column
#'   for each year in the data, i.e 55 columns.
#'   The names of the countries are given in the row names of the dataset,
#'   e.g. by \code{rownames(popn)}.
#' @source The World Bank's
#' \href{https://data.worldbank.org/data-catalog/world-development-indicators}{World Development Indicators database}.
#' @name world_bank
NULL
## NULL

#' @rdname world_bank
"co2_per_capita"

#' @rdname world_bank
"gdp_per_capita"

#' @rdname world_bank
"population_size"

#' @rdname world_bank
"life_expectancy"

#' Australian Birth Times Data
#'
#' The baby arrivals data introduced in Chapter 6 of the STAT0002 notes.
#' Information concerning the 44 babies (18 girls and 26 boys) born in a
#' 24-hour period at the Mater Mothers' Hospital, Brisbane, Australia, on
#' December 18, 1997.
#'
#' @format A data frame with 44 observations on the following 3 variables.
#' \itemize{
#' \item{\code{time:}}{ the number of minutes past midnight at which the
#'   baby was born.}
#' \item{\code{sex:}}{ a character variable, the sex of the baby ("girl"
#'   or "boy").}
#' \item{\code{weight:}}{ the weight of the baby in grams.}
#' }
#' @source Steele, S. (December 21, 1997), Babies by the Dozen for Christmas:
#' 24-Hour Baby Boom, The Sunday Mail (Brisbane), page 7.
"aussie_births"

#' Nebulae data from Hubble (1929)
#'
#' The data used in Section 9.1 of the STAT0002 notes to introduce simple
#' linear regression.  This dataset contains estimates of the distance
#' from Earth and the velocity relative to Earth of 24 nebulae.
#' @format A data frame with 24 observations on the following 2 variables.
#' \itemize{
#' \item{\code{distance:}}{ the distance, in megaparsecs, of the nebula
#'   from Earth.}
#' \item{\code{velocity:}}{ the velocity, in kilometres per second, of the
#'   nebula relative to Earth.}
#' }
#' @source Hubble, E. (1929) A relation between distance and radial velocity
#' among extra-galactic nebulae. \emph{Proceedings of the National Academy of
#' Science}, \strong{15}, 168-173.
"hubble"

#' Nebulae data from Freedman (2001)
#'
#' Data that are an update to the Hubble (1929) \code{\link{hubble}}
#' data. Contains estimates of the distance from Earth and the velocity
#' relative to Earth of 24 nebulae.
#' @format A data frame with 24 observations on the following 2
#' variables.
#' \itemize{
#' \item{\code{distance:}}{ the distance, in megaparsecs, of the nebula
#'   from Earth.}
#' \item{\code{velocity:}}{ the velocity, in kilometres per second, of the
#'   nebula relative to Earth.}
#' }
#' @source The velocity values are from column 7 of Table 5 of Freedman (2001)
#' and the distance values are the corresponding values from Table 4.
#' @references Freedman et al. (2001) Final results from the Hubble space
#' telescope key project to measure the Hubble constant.
#' \emph{The Astrophysical Journal}, \strong{553}, 47-72.
"hubble2"

#' Cloud-seeding data
#'
#' The data used in the STAT0002 notes to introduce the idea of transforming
#' data to approximate symmetry.  This dataset contains the amounts of
#' rainfall, in acre-feet, produced by 52 clouds.  Half of the clouds were
#' chosen at random to be seeded with silver nitrate.
#' @format A data frame with 26 observations on the following 2 variables.
#' \itemize{
#' \item{\code{unseeded:}}{ the rainfall amount for unseeded clouds.}
#' \item{\code{seeded:}}{ the rainfall amount for unseeded clouds.}
#' }
#' @source Simpson, J., Olsen, A. , and Eden. J.C. (1975). A Bayesian analysis
#' of a multiplicative treatment effect in weather modification.
#' \emph{Technometrics}, \strong{17}, 161-166.
"clouds"

# ========================== Bird dominance data =========================

#' Between-species dominance in birds data
#'
#' Francis et al. (2018) conducted an experiment to study the competitive
#' interactions between 10 species of birds.  These are the data presented in
#' \href{https://doi.org/10.1371/journal.pone.0202152.t001}{Table 1} of this paper.
#'
#' @format A data frame with 10 observations on the following 8 variables.
#' \itemize{
#' \item{\code{species:}}{ a two-letter abbreviation for the bird species.}
#' \item{\code{species_name:}}{ the common bird species name.}
#' \item{\code{sci_name:}}{ the scientific bird species name.}
#' \item{\code{mass_g:}}{ mean species mass in grams.}
#' \item{\code{dom_rank:}}{ dominance rank, the smaller the value the more
#'   dominant species tends to be over other species.}
#' \item{\code{dom_rank_LCI:}}{ lower 95\% confidence limit for the dominance
#'   rank.}
#' \item{\code{dom_rank_UCI:}}{ upper 95\% confidence limit for the dominance
#'   rank.}
#' }
#' @source \href{https://doi.org/10.1371/journal.pone.0202152.s002}{S1 dataset.}
#'   Data for species identities, mass and dominance rankings from
#'   Francis et al (2018).
#' @references Francis M.L., Plummer K.E., Lythgoe B.A., Macallan C., Currie T.E.,
#'   Blount J.D. (2018) Effects of supplementary feeding on interspecific
#'   dominance hierarchies in garden birds. PLoS ONE 13(9): e0202152.
#'   \href{https://doi.org/10.1371/journal.pone.0202152}{https://doi.org/10.1371/journal.pone.0202152}
"birds"

# === Tokyo 2020 Olympics Diving Men's Individual 10m platform final results ==

#' Tokyo 2020 Olympics Men's Individual 10m platform
#'
#' Detailed results from the final of the Men's Individual 10m platform final
#' at the Tokyo 2020 Olympic Games.
#'
#' @format A data frame with 60 observations on the following 17 variables.
#'   Each of the 10 competitors has one row of data for each of their 6 dives.
#'   For each competitor, the first row relates to the first round of dives
#'   and the sixth row the last round.
#' \itemize{
#' \item{\code{Rank:}}{ Final position (rank) in competition.}
#' \item{\code{Name:}}{ Full name.}
#' \item{\code{NOCcode:}}{ Country code.}
#' \item{\code{DiveNo:}}{ Code for the type of dive performed.}
#' \item{\code{DD:}}{ Degree of difficulty: the larger the number the more
#'   difficult the dive.}
#' \item{\code{J1-J7:}}{ Respective scores from each of the 7 judges.}
#' \item{\code{DivePoints:}}{ Points awarded for the dive, based on the scores
#'   and the degree of difficulty.}
#' \item{\code{DiveRank:}}{ The rank of the dive in the round in question.  In
#'   the raw data there are entries like "=2", resulting from ties.  In
#'   \code{daley} the "=" sign is removed and the ranks are stored as numeric
#'   scalars, e.g. 2.}
#' \item{\code{TotalPoints:}}{ The total point accumulated from all dives
#'   to this round of the competition.}
#' \item{\code{OverallRank:}}{ The rank of the diver after the current round.
#'   In the raw data there are entries like "=2", resulting from ties.  In
#'   \code{daley} the "=" sign is removed and the ranks are stored as numeric
#'   scalars, e.g. 2.}
#' \item{\code{PointsBehind:}}{ The number of points that the diver is behind
#'   the leader after the current round.  If the diver is the leader then this
#'   is missing (NA).}
#' }
#' @details \code{DivePoints} is calculated by: removing the 2 smallest and 2
#'   largest scores from the 7 scores \code{J1-J7}; calculating the sum of the
#'   3 scores that remain; and multiplying the result by \code{DD}.
#' @source \href{https://olympics.com/tokyo-2020/olympic-games/en/results/diving/olympic-schedule-and-results.htm}{Tokyo 2020 Diving Results}, specifically
#' \href{https://olympics.com/tokyo-2020/olympic-games/en/results/diving/results-men-s-10m-platform-fnl-000100-.htm}{Men's 10m Platform Results}.
"daley"

# =========================== Kerrich's coin data =============================

#' Kerrich's coin data
#'
#' A summary of the 10,000 coin throws conducted by Jon Kerrich.
#'
#' @format A data frame with 35 observations on 2 variables:
#' \itemize{
#'   \item{\code{throws:}}{ the number of throws conducted.}
#'   \item{\code{heads:}}{ the corresponding cumulative number of heads
#'     obtained.}
#' }
#' @details These data are considered in
#' \href{https://paulnorthrop.github.io/stat0002book/probability.html#relative-frequency-definition-of-probability}{Section 3.2}
#'   of the STAT0002 notes.
#' @references Kerrich, J. E. (1946). An Experimental Introduction to the Theory
#'   of Probability. E. Munksgaard.
#' @examples
#' # This code produces the plot in Figure 3.1 of the STAT0002 notes
#' plot(kerrich$throws, kerrich$heads / kerrich$throws,
#'      ylab = "proportion of heads",
#'      xlab = "number of throws (logarithmic scale)", lwd = 2, type = "l",
#'      log = "x", ylim = c(0,1), axes = FALSE)
#' abline(h = 0.5, lty = 2)
#' axis(1, labels = as.character(c(3, 10, 30, 100, 300, 1000, 3000, 10000)),
#'      at=c(3, 10, 30, 100, 300, 1000, 3000, 10000))
#' axis(2, labels = c(0, 0.2, 0.4, 0.5, 0.6, 0.8, 1.0),
#'      at=c(0, 0.2, 0.4, 0.5, 0.6, 0.8, 1.0))
"kerrich"

# ========================== Berkeley Admissions data =========================

#' Student Admisions at UCL Berkeley
#'
#' Aggregate data on applicants to graduate school at Berkeley for the six
#' largest departments in 1973 classified by admission and sex.
#' @details This is a copy of the \code{\link[datasets]{UCBAdmissions}} data
#'   provided in the \code{\link[datasets:datasets-package]{datasets}} package.
"berkeley"

# ========================== Berkeley Admissions data =========================

#' UK/US and UK/Canada Exchange Rates
#'
#' The \code{exchange} data frame has 975 rows and 2 columns. The columns
#' contain daily exchange rates; UK sterling against the US dollar (first
#' column) and UK sterling against the Canadian dollar (second column). The
#' rownames contain the corresponding dates in a character string with the
#' format \code{"2000/05/26"}. This can be converted into a \code{POSIXct} or
#' \code{POSIXlt} object using \code{\link{as.POSIXct}} or
#' \code{\link{as.POSIXlt}}.
#'
#' @format This data frame contains the following columns:
#' \itemize{
#'   \item{USD.GBP:}{US against UK exchange rate.}
#'   \item{CAD.GBP:}{Canada against UK exchange rate.}
#' }
#' @source Coles, S. G. (2001) \emph{An Introduction to Statistical Modelling
#'   of Extreme Values.} London: Springer.
#' @examples
#' # This produces a plot like Figure 10.1 of the STAT0002 notes
#' plot(exchange)
#'
#' # The produces a plot like Figure 10.2 of the STAT0002 notes
#' # Calculate the log-returns
#' USDlogr <- diff(log(exchange$USD.GBP))
#' CADlogr <- diff(log(exchange$CAD.GBP))
#' plot(USDlogr, CADlogr)
"exchange"
