# =================== Calculation of average diving scores ===================

#' Calculation of diving scores
#'
#' Functions to implement four ways to calculate the points awarded for a dive
#' in a diving competition, including the way that is used in practice.
#'
#' @param x A numeric matrix or data frame, ideally of the same format as
#'   \code{\link{daley1}}.
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
#' @param replace A logical scalar.  If \code{replace = FALSE} then a vector
#'   of the dive points is returned. If \code{replace = TRUE} then the
#'   \code{DivePoints}, \code{DiveRank}, \code{TotalPoints}, \code{OverallRank}
#'   and \code{PointsBehind} columns are updated in light of the new values of
#'   the dive points and this data frame is returned.  If any of these named
#'   columns do not exist then the returned object is the same as the
#'   \code{replace = FALSE} case.
#' @details In the default case, e.g. \code{dmean(daley1)}, the 2 smallest and
#'   2 largest scores of the 7 scores are removed; the sum of the 3 remaining
#'   scores is calculated; and the result is multiplied by the dive's
#'   degree of difficulty.
#'
#'   Another way to think of this is that we take the sample mean of the 3
#'   middle scores, then multiply by 3, then multiply by the degree of
#'   difficulty.
#'
#'   In the \code{dmean(daley1, trim = 0)} case we take the sample mean of all
#'   7 scores, then multiply by 3, then multiply by the degree of difficulty.
#'   That is, we do not trim the 2 smallest and 2 largest values.
#'
#'   Similarly, in the \code{dmedian(daley1)} and \code{dmode(daley1)} cases we
#'   take the sample median or mode, respectively, of all 7 scores, then
#'   multiply by 3, then multiply by the degree of difficulty. If there is more
#'   than one sample mode then we use the sample mean of these modes.
#' @return A numeric vector dive points if \code{replace = FALSE} or \code{x}
#'   does not have appropriately named columns.  Otherwise, a dataframe of the
#'   same structure as \code{x} in which the columns \code{DivePoints},
#'   \code{DiveRank}, \code{TotalPoints}, \code{OverallRank} and
#'   \code{PointsBehind} have been updated in light of the new dive points
#'   values.
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

replace_fn <- function(x, divePoints) {
  cols <- c("DivePoints", "DiveRank", "TotalPoints", "OverallRank",
            "PointsBehind")
  # Return the dive points if x does not have columns with the correct names
  if (!all(cols %in% colnames(x))) {
    return(x)
  }
  # DivePoints
  x[, "DivePoints"] <- divePoints
  # Number of dives, divers, dives per diver (number of rounds)
  n <- nrow(x)
  fullName <- paste0(x[, "Surname"], x[, "FirstName"])
  nd <- length(unique(fullName))
  nper <- n / nd
  theRound <- rep(1:nper, times = nd)
  theDiver <- rep(1:nd, each = nper)
  # Row i contains the cumulative points (TotalPoints) diver i
  temp <- aggregate(x[, "DivePoints"], by = list(theDiver), FUN = cumsum)[, -1]
  # TotalPoints
  x[, "TotalPoints"] <- c(t(temp))
  # DiveRank
  # We negate x[, "DivePoints] because we want the larger values to have the
  # lower rank
  # Column i contain the DiveRanks for diver number i
  temp <- aggregate(-x[, "DivePoints"], by = list(theRound),
                    FUN = rank, ties.method = "min")[, -1]
  x[, "DiveRank"] <- c(temp)
  # OverallRank
  # Column i contain the OverallRanks for diver number i
  temp <- aggregate(-x[, "TotalPoints"], by = list(theRound),
                    FUN = rank, ties.method = "min")[, -1]
  x[, "OverallRank"] <- c(temp)
  # PointsBehind
  # Find the maximum current total by round
  # temp is a vector of length nper (number of rounds)
  temp <- aggregate(x[, "TotalPoints"], by = list(theRound),
                    FUN = max)[, -1]
  temp <- rep_len(temp, length.out = n)
  diffs <- temp - x[, "TotalPoints"]
  diffs[diffs == 0 ] <- NA
  # temp - by-round scores for each diver
  x[, "PointsBehind"] <- diffs
  return(x)
}

# =========================== (Trimmed) median-based ==========================

#' @rdname diving
#' @export
dmean <- function(x, DD = "DD", scores = paste0("J", 1:7), trim = 2 / 7,
                  replace = FALSE) {
  # Extract a matrix of the scores
  y <- x[, scores]
  # Calculate the trimmed row means, multiply by 3 to get the trimmed sums
  # and multiply by the degree of difficulty
  res <- x[, DD] * apply(y, 1, mean, trim = trim) * 3
  if (replace) {
    res <- replace_fn(x, res)
  }
  return(res)
}

# ================================= Median-based ==============================

#' @rdname diving
#' @export
dmedian <- function(x, DD = "DD", scores = paste0("J", 1:7), type = 6,
                    replace = FALSE) {
  # Extract a matrix of the scores
  y <- x[, scores]
  # Calculate the row medians, multiply by 3 and multiply by the degree of
  # difficulty
  res <- x[, DD] * apply(y, 1, quantile, type = type) * 3
  if (replace) {
    res <- replace_fn(x, res)
  }
  return(res)
}

# ================================== Mode-based ===============================

#' @rdname diving
#' @export
dmode <- function(x, DD = "DD", scores = paste0("J", 1:7), replace = FALSE) {
  # Extract a matrix of the scores
  y <- x[, scores]
  # Calculate the ow modes (or the sample mean of these if there is more than
  # one), multiply by 3 and multiply by the degree of difficulty
  mode_fn <- function(x) {
    tab <- table(x)
    vals <- as.numeric(names(tab))
    wm <- which.max(tab)
    y <- vals[tab == tab[wm]]
    return(mean(y))
  }
  res <- x[, DD] * apply(y, 1, mode_fn) * 3
  if (replace) {
    res <- replace_fn(x, res)
  }
  return(res)
}

