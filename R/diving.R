# =================== Calculation of average diving scores ===================

#' Calculation of diving scores
#'
#' Functions to implement four ways to calculate the points awarded for a dive
#' in a diving competition, including the way that is used in the Olympic
#' Games.
#'
#' @param x A numeric matrix or data frame, ideally of the same format as
#'   \code{\link{daley}}.
#' @param DD A character (or numeric) scalar giving the column name (or number)
#'   of \code{x} that contains the DD (degree of difficulty) values of each
#'   dive.  The default is set up to work with the data in
#'   \code{\link{daley}}.
#' @param scores A character (or numeric) vector giving the column names (or
#'   numbers) of \code{x} that contain the 7 scores for each dive.  The default
#'   is set up to work with the data in \code{\link{daley}}.
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
#' @param reorder A logical scalar.  If \code{replace = TRUE} then
#'   \code{reorder} determines whether the order of the divers is changed
#'   to reflect the new values of \code{TotalPoints} at the end of the
#'   competition.
#' @param type Determines the form of a data frame returned as a summary table.
#' \itemize{
#'   \item {\code{type = 1}: DD, J1-J7 and dive points (those already in
#'     \code{x}).}
#'   \item {\code{type = 2}: like \code{type = 1} but gives dive points, and
#'     points based on the mean, median and mode.}
#'   \item {\code{type = 3}: like \code{type = 2} but gives only the total
#'     points accumulated over the competition, ordered by dive points.}
#'   \item {\code{type = 4}: like \code{type = 3} but totals are replaced by
#'     ranks.}
#' }
#' @param diverRanks The ranks of the divers that should be included in a
#'   table. For example, \code{diverRanks} gives the medalists.
#' @details In the default case, e.g. \code{dmean(daley)}, the 2 smallest and
#'   2 largest scores of the 7 scores are removed; the sum of the 3 remaining
#'   scores is calculated; and the result is multiplied by the dive's
#'   degree of difficulty.
#'
#'   Another way to think of this is that we take the sample mean of the 3
#'   middle scores, then multiply by 3, then multiply by the degree of
#'   difficulty.
#'
#'   In the \code{dmean(daley, trim = 0)} case we take the sample mean of all
#'   7 scores, then multiply by 3, then multiply by the degree of difficulty.
#'   That is, we do not trim the 2 smallest and 2 largest values.
#'
#'   Similarly, in the \code{dmedian(daley)} and \code{dmode(daley)} cases we
#'   take the sample median or mode, respectively, of all 7 scores, then
#'   multiply by 3, then multiply by the degree of difficulty. If there is more
#'   than one sample mode then we use the sample mean of these modes.
#' @return A numeric vector dive points if \code{replace = FALSE} or \code{x}
#'   does not have appropriately named columns.  Otherwise, a dataframe of the
#'   same structure as \code{x} in which the columns \code{DivePoints},
#'   \code{DiveRank}, \code{TotalPoints}, \code{OverallRank} and
#'   \code{PointsBehind} have been updated in light of the new dive points
#'   values.
#' @seealso \code{\link{daley}} for Tokyo 2020 Olympics Diving Men's
#'   Individual 10m platform final results
#' @examples
#' # The calculation used in competitions (trimmed sample sums)
#' dmean(daley, replace = FALSE)
#'
#' # An equivalent calculation without trimming
#' dmean(daley, replace = FALSE, trim = 0)
#'
#' # An equivalent calculation based on a sample median
#' dmedian(daley, replace = FALSE)
#'
#' # An equivalent calculation based on a sample mode
#' dmode(daley, replace = FALSE)
#' @name diving
NULL
## NULL

replace_fn <- function(x, divePoints) {
  cols <- c("DivePoints", "DiveRank", "TotalPoints", "OverallRank",
            "PointsBehind")
  # Return the dive points if x does not have columns with the correct names
  if (!all(cols %in% colnames(x))) {
    return(divePoints)
  }
  # DivePoints
  x[, "DivePoints"] <- divePoints
  # Number of dives, divers, dives per diver (number of rounds)
  n <- nrow(x)
  fullName <- x[, "Name"]
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

reorder_fn <- function(x) {
  # Number of dives, divers, dives per diver (number of rounds)
  n <- nrow(x)
  fullName <- x[, "Name"]
  nd <- length(unique(fullName))
  nper <- n / nd
  theRound <- rep(1:nper, times = nd)
  theDiver <- rep(1:nd, each = nper)
  # Find the largest TotalPoints value for each diver
  tp <- x[, "TotalPoints"]
  dtp <- aggregate(tp, by = list(theDiver), FUN = max)[, -1]
  newOrder <- rank(-dtp)
  newOrder <- nper * (rep(newOrder, each = nper) - 1) + 1:6
  return(x[newOrder,])
}

positions <- function(x, positions = 1:3) {
  x[x[, "Rank"] %in% positions, ]
}

overall <- function(x, which = c("points", "mean", "median", "mode")) {
  p <- dmean(x)
  mn <- dmean(x, trim = 0)
  mdn <- dmedian(x)
  md <- dmode(x)
  cbind(points = p, mean = mn, median = mdn, mode = md)
}


# =========================== (Trimmed) median-based ==========================

#' @rdname diving
#' @export
dmean <- function(x, DD = "DD", scores = paste0("J", 1:7), trim = 2 / 7,
                  replace = TRUE, reorder = TRUE) {
  # Extract a matrix of the scores
  y <- x[, scores]
  # Calculate the trimmed row means, multiply by 3 to get the trimmed sums
  # and multiply by the degree of difficulty
  res <- x[, DD] * apply(y, 1, mean, trim = trim) * 3
  if (replace) {
    res <- replace_fn(x, res)
    if (reorder) {
      res <- reorder_fn(res)
    }
  }
  if (trim == 0) {
    attr(res, "statistic") <- "MeanPoints"
  } else {
    attr(res, "statistic") <- "DivePoints"
  }
  return(res)
}

# ================================= Median-based ==============================

#' @rdname diving
#' @export
dmedian <- function(x, DD = "DD", scores = paste0("J", 1:7), type = 6,
                    replace = TRUE, reorder = TRUE) {
  # Extract a matrix of the scores
  y <- x[, scores]
  # Calculate the row medians, multiply by 3 and multiply by the degree of
  # difficulty
  res <- x[, DD] * apply(y, 1, quantile, probs = 0.5, type = type) * 3
  if (replace) {
    res <- replace_fn(x, res)
    if (reorder) {
      res <- reorder_fn(res)
    }
  }
  attr(res, "statistic") <- "MedianPoints"
  return(res)
}

# ================================== Mode-based ===============================

#' @rdname diving
#' @export
dmode <- function(x, DD = "DD", scores = paste0("J", 1:7), replace = TRUE,
                  reorder = TRUE) {
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
    if (reorder) {
      res <- reorder_fn(res)
    }
  }
  attr(res, "statistic") <- "ModePoints"
  return(res)
}

#' @rdname diving
#' @export
tables <- function(x, type = 1, diverRanks = 1) {
  # Types
  # 1: DD, J1-J7 and dive points (those already in x) for a subset of divers
  # 2: like type 1 but give DivePoints, MeanPoints, MedianPoints, ModePoints
  # 3: like type 2 but give the total points, ordered by DivePoints
  # 4: like 3 but replace TotalPoints with rank
  theNames <- c("DivePoints", "MeanPoints", "MedianPoints", "ModePoints")
  whichCol <- which(colnames(x) %in% theNames)
  whichColName <- colnames(x)[whichCol]
  if (type == 1) {
    cols <- c("Name", "DD", paste0("J", 1:7), whichColName)
    rows <- which(x[, "Rank"] %in% diverRanks)
    res <- x[rows, cols]
    if (!is.null(attributes(x)$statistic)) {
      names(res)[names(res) == "DivePoints"] <- attributes(x)$statistic
    }
    # Remove instances of "Points", to save space
    names(res) <- sub("Points", "", names(res))
  } else if (type == 2) {
    # Save the existing points column and then remove it (later)
    savePoints <- x[, whichCol]
    # Calculate the new points
    p <- dmean(x, replace = FALSE)
    mn <- dmean(x, replace = FALSE, trim = 0)
    mdn <- dmedian(x, replace = FALSE)
    md <- dmode(x, replace = FALSE)
    x <- x[, -whichCol]
    addMat <- cbind(p, mn, mdn, md)
    colnames(addMat) <- theNames
    x <- cbind(x, addMat)
    cols <- c("Name", "DD", paste0("J", 1:7), theNames)
    rows <- which(x[, "Rank"] %in% diverRanks)
    res <- x[rows, cols]
    # Remove instances of "Points", to save space
    names(res) <- sub("Points", "", names(res))
    # Round numeric values to 2 d.p.
    res <- cbind(res[, 1], round(res[, -1], 2))
  } else if (type == 3 || type == 4) {
    # Calculate the new points, using replace = TRUE and ordering on DivePoints
    p <- dmean(x, reorder = TRUE)
    mn <- dmean(x, trim = 0, reorder = FALSE)
    mdn <- dmedian(x, reorder = FALSE)
    md <- dmode(x, reorder = FALSE)
    #
    n <- nrow(p)
    fullName <- p[, "Name"]
    nd <- length(unique(fullName))
    nper <- n / nd
    theRound <- rep(1:nper, times = nd)
    theDiver <- rep(1:nd, each = nper)
    # Find the largest TotalPoints value for each diver
    findTotal <- function(xx) {
      tp <- xx[, "TotalPoints"]
      dtp <- aggregate(tp, by = list(theDiver), FUN = max)[, -1]
    }
    pTotal <- findTotal(p)
    mnTotal <- findTotal(mn)
    mdnTotal <- findTotal(mdn)
    mdTotal <- findTotal(md)
    res <- data.frame("Name" = unique(fullName), "Dive" = pTotal,
                      "Mean" = mnTotal, "Median" = mdnTotal, "Mode" = mdTotal)
    if (type == 4) {
      for (i in 2:5) {
        res[, i] <- rank(-res[, i])
      }
    }
    res <- res[diverRanks, ]
  }
  return(res)
}
