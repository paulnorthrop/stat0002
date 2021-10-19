# =================== Calculation of average diving scores ===================

#' Calculation of diving scores
#'
#' Functions to implement four ways to calculate a score for a dive in a
#' diving competitions, including the way that is used in practice.
#'
#' @param x A numeric matrix or data frame.
#' @param DD A character (or numeric) scalar giving the column name (or number)
#'   of \code{x} that contains the DD (degree of difficulty) values of each
#'   dive.  The default is set up to work with the data in
#'   \code{\link{daley1}}.
#' @param scores A character (or numeric) vector giving the column names (or
#'   numbers) of \code{x} that contain the 7 scores for each dive.  The default
#'   is set up to work with the data in \code{\link{daley1}}.
#' @param trim Only relevant to \code{divemean}. Passed to \code{\link{mean}}.
#'   The default, \code{trim = 2 / 7}, corresponds to the calculation using in
#'   diving competitions.
#' @param type Only relevant to \code{divemedian}.  Passed to
#'   \code{\link[stats]{quantile}}.  The default, \code{type = 6}, corresponds
#'   to the method that we have studied in STAT0002.
#' @details In the default case, e.g. \code{dmean(daley1)}, the 2 smallest and
#'   2 largest scores of the 7 scores are removed; the sum of the 3 remaining
#'   scores is calculated; and the result is multiplied by the dive's
#'   degree of difficulty.
#'
#'   Another way to think of this is that we take the
#'   sample mean of the 3 middle scores, then multiply by 3, then multiply by
#'   the degree of difficulty.
#'
#'   In the \code{dmean(daley1, trim = 0)} case we take the sample mean of all
#'   7 scores, then multiply by 3, then multiply by the degree of difficulty.
#'   That is, we do not trim the 2 smallest and 2 largest values.
#'
#'   Similarly, in the \code{dmedian(daley1, trim = 0)} and
#'   \code{dmode(daley1, trim = 0)} we case we take the sample median or mode,
#'   respectively, of all 7 scores, then multiply by 3, then multiply by the
#'   degree of difficulty.
#' @seealso \code{\link{daley1}} for Tokyo 2020 Olympics Diving Men's
#'   Individual 10m platform final results
#' @examples
#' # The calculation used in competitions (trimmed sample sums)
#' dmean(daley1)
#'
#' # An equivalent calculation without trimming
#' dmean(daley1, trim = 0)
#'
#' # An equivalent calculation based on a sample median
#' dmedian(daley1)
#'
#' # An equivalent calculation based on a sample mode
#' dmode(daley1)
#' @name diving
NULL
## NULL


# ============================ (Trimmed) medan-based ==========================

#' @rdname diving
#' @export
dmean <- function(x, DD = "DD", scores = paste0("J", 1:7), trim = 2 / 7) {
  # Extract a matrix of the scores
  y <- x[, scores]
  # Calculate the trimmed row means, multiply by 3 to get the trimmed sums
  # and multiply by the degree of difficulty
  x[, DD] * apply(y, 1, mean, trim = trim) * 3
}

# ================================= Median-based ==============================

#' @rdname diving
#' @export
dmedian <- function(x, DD = "DD", scores = paste0("J", 1:7), type = 6) {
  # Extract a matrix of the scores
  y <- x[, scores]
  # Calculate the trimmed row means, multiply by 3 to get the trimmed sums
  # and multiply by the degree of difficulty
  x[, DD] * apply(y, 1, quantile, type = type) * 3
}

# ================================== Mode-based ===============================

#' @rdname diving
#' @export
dmode <- function(x, DD = "DD", scores = paste0("J", 1:7)) {
  # Extract a matrix of the scores
  y <- x[, scores]
  # Calculate the trimmed row means, multiply by 3 to get the trimmed sums
  # and multiply by the degree of difficulty
  x[, DD] * apply(y, 1, mode) * 3
}

