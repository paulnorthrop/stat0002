#' stat0002: Introduction to Probability and Statistics at UCL
#'
#' Provides example datasets, R code and tutorials to help students taking
#' STAT0002 Introduction to Probability and Statistics at
#' University College London to understand the course material and to see how
#' R can be used to perform some of the analyses in the course.
#' This package can also be used to complement STAT0004 Introduction to
#' Practical Statistics, enabling students to become more familiar with R
#' code by seeing it in action and playing with it themselves.
#' A useful source of information is the
#' \href{https://paulnorthrop.github.io/stat0002/}{stat0002 home page on GitHub}.
#'
#' @section Please note:
#' \strong{The main source of information about the material in STAT0002 is
#' the
#' \href{https://moodle.ucl.ac.uk/course/view.php?id=8579&section=0}{STAT0002 Moodle page}.
#' You are not required to use the `stat0002` package during STAT0002, but it
#' is recommended as a supplement to the STAT0002 syllabus.}
#'
#' @section How to learn about R:
#' The best way to learn how to use a computer program is to play with code and
#' see what it does.  R is based on \emph{functions} that look like \code{name()}, where
#' \code{name} is the name of the function and where various \emph{arguments} are inserted
#' inside the brackets \code{()}.  In the R code provided with this package you will
#' see many functions like this.  To find out what a function called \code{name} does
#' use either \code{?name} or \code{help(name)}.
#'
#' If you have any questions about this package please ask them via the
#' \href{https://moodle.ucl.ac.uk/mod/hsuforum/view.php?id=866683}{STAT0002 Moodle Discussion Forum}.
#'
#' @section Vignettes (tutorials demonstrating how to use R code):
#'
#' \itemize{
#'   \item {\href{../doc/stat0002-ch1a-shuttle-vignette.html}{Chapter 1: Challenger Space Shuttle Disaster}}
#'   \item {\href{../doc/stat0002-ch1b-stochastic-simulation-vignette.html}{Chapter 1: Stochastic Simulation}}
#'   \item {\href{../doc/stat0002-ch2a-descriptive-statistics-vignette.html}{Chapter 2: Descriptive Statistics}}
#'   \item {\href{../doc/stat0002-ch2b-graphs-vignette.html}{Chapter 2: Graphs (One Variable)}}
#'   \item {\href{../doc/stat0002-ch2c-graphs-vignette.html}{Chapter 2: Graphs (More Than One Variable)}}
#'   \item {\href{../doc/stat0002-ch2d-box-cox-transformation-vignette.html}{Chapter 2: Transformation Of Variable}}
#'   \item {\href{../doc/stat0002-ch5a-discrete-random-variables.html}{Chapter 5: binomial, geometric and Poisson distributions}}
#'   \item {\href{../doc/stat0002-ch5b-continuous-random-variables.html}{Chapter 5: uniform, exponential and normal distributions}}
#' }
#'
#' @section Datasets:
#'
#' \itemize{
#'   \item {\link[=shuttle]{Challenger Space Shuttle Disaster}}
#'   \item {\link[=days]{Mystery data (in Exercises 1)}}
#'   \item {\link[=lymphoma]{T-cell count data}}
#'   \item {\link[=clouds]{Cloud-seeding data}}
#'   \item {\link[=ox_births]{Oxford Birth Times}}
#'   \item {\link[=blood_types]{Blood Types}}
#'   \item {\link[=ftse]{FTSE 100 Share Index}}
#'   \item {\link[=world_bank]{World Bank Development Indicators}}
#'   \item {\link[=flu]{Influenza data}}
#'   \item {\link[=UCBAdmissions]{Student Admissions at UC Berkeley}}
#'   \item {\link[=USelection]{The 2000 U.S. Presidential Election}}
#'   \item {\link[=aussie_births]{Australian Birth Times Data}}
#'   \item {\link[=hubble]{Nebulae data from Hubble (1929)}}
#'   \item {\link[=birds]{Between-species dominance in birds}}
#' }
#'
#' @section \link[=stat0002movies]{Movies}:
#'
#' Some of the following movies are made available via the
#' \code{\link[smovie]{smovie}} package.  You can access a menu for the
#' movies in smovie using \code{library(smovie)} and then \code{movies()}.
#' See \code{\link[smovie]{movies}} for more details.
#'
#' \itemize{
#'   \item {\link[=shuttle_movie]{Challenger Space Shuttle Disaster}}
#'   \item {\link[=scatterplot_movie]{US election: straightening scatter
#'     plots}}
#'   \item {\link[=world_bank_movie]{World Bank Development Indicators}}
#'   \item {\link[=ox_births_movie]{Oxford Birth Times Simulation}}
#'   \item {\link[=binomial_pmf_movie]{Binomial p.m.f.}}
#'   \item {\link[=poisson_process_movie]{Poisson process}}
#'   \item {\link[=poisson_process_check]{Graphical checking of Poisson process properties}}
#'   \item {\link[=normal_pdf_movie]{Normal probability density function}}
#'   \item {\link[=normal_areas_movie]{Normal central probability areas}}
#'   \item {\link[=qq_plot_movie]{Construction of a (normal) QQ plot}}
#'   \item {\link[=normal_sampling_distns_movie]{Normal sampling distributions}}
#'   \item {\link[=clt_normal_movie]{Central Limit Theorem: normal data}}
#'   \item {\link[=clt_exponential_movie]{Central Limit Theorem: exponential data}}
#'   \item {\link[=mean_vs_median_normal_movie]{Mean vs median for a normal random sample}}
#'   \item {\link[=mean_vs_median_t_movie]{Mean vs median for a Student's t random sample}}
#'   \item {\link[=two_by_two_movie]{Test for lack of association in a 2 by 2 contingency table}}
#'   \item {\link[=lin_reg_movie]{Simple linear regression using least squares estimation}}
#'   \item {\link[=lev_inf_movie]{Leverage and influence in simple linear regression}}
#'   \item {\link[=corr_sim_movie]{Sampling distribution of the correlation coefficient}}
#' }
#' @seealso \code{\link{stat0002movies}}: general information about the movies.
#' @seealso The \href{https://moodle.ucl.ac.uk/course/view.php?id=8579&section=0}{STAT0002 Moodle page}
#'   for: various forms of feedback, a discussion forum, lecturecast recordings,
#'   course notes, exercises, quizzes, short videos, past exam papers and revision
#'   lecture material, real investigations.
#' @seealso The \href{https://moodle.ucl.ac.uk/course/view.php?id=8581}{STAT0004 Moodle page}
#'   (for students taking STAT0004).
#' @seealso The \href{https://moodle.ucl.ac.uk/course/view.php?id=10773}{Introduction to R Moodle page}
#'   (for students not taking STAT0004, e.g. Natural Sciences students).
#' @docType package
#' @name stat0002
#' @importFrom stats runif
#' @importFrom graphics plot
#' @importFrom distributions3 cdf
#' @importFrom rust ru
#' @importFrom smovie movies
#' @import rpanel
#' @aliases stat0002
NULL

#' Challenger Space Shuttle Disaster Dataset
#'
#' These data are discussed in the first STAT0002 lecture.
#' A paper (Dalal \emph{et al.}, 1989), in which these data are analysed
#' with a view to estimating (retrospectively) the probability of a
#' catastrophic failure of the Challenger space shuttle under the
#' launch conditions on 28th January 1986 is used as a worked example
#' for the STAT0002 Meet your Professor ICA.
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
#' @seealso \code{\link{shuttle_sim}}, \code{\link{shuttle_sim_plot}},
#'   \code{\link{shuttle_movie}}
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
#' The baby arrivals data introduced in Section 5 of the STAT0002 notes.
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
#' The data used in Section 8.1 of the STAT0002 notes to introduce simple
#' linear regression.  This dataset contains estimates of the distance
#' from Earth and the velocity relative to Earth of 24 nebulae.
#' @format A data frame with 42 observations on the following 2 variables.
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

