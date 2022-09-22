# ================================ box_plot ===============================

#' Box plots
#'
#' Produce box-and-whisker plot(s) of the given (grouped) values, with an
#' option (using the argument \code{type}) to change the estimator used
#' to estimate the quartiles.  The only argument described below is
#' \code{type}.  For details of the other arguments see the
#' \strong{Arguments} section of \code{\link[graphics]{boxplot}}.
#'
#' @section \strong{Usage}:
#'   See the \strong{Usage} section of the \code{\link[graphics]{boxplot}}
#'   documentation.
#'
#' @param type an integer between 1 and 9 selecting one of the nine quantile
#'   algorithms detailed in the \strong{Details} section of the
#'   \code{\link[stats]{quantile}} documentation.  The default is
#'   \code{type = 6}.
#' @details See the \strong{Details} section of the \code{\link{boxplot}}
#'   documentation.
#' @return A list with the same contents as described in the \strong{Value}
#'   section of the \code{\link{boxplot}} documentation.
#' @examples
#' birth_times <- ox_births[, "time"]
#' box_plot(birth_times)
#' box_plot(birth_times, horizontal = TRUE)
#' @name box_plot
NULL
## NULL

#' @export
box_plot <- function(x, ...) {
  UseMethod("box_plot")
}

#' @export
box_plot.default <- function (x, ..., range = 1.5, width = NULL,
                              varwidth = FALSE, notch = FALSE, outline = TRUE,
                              names, plot = TRUE, border = graphics::par("fg"),
                              col = NULL, log = "",
                              pars = list(boxwex = 0.8, staplewex = 0.5,
                                          outwex = 0.5), horizontal = FALSE,
                              add = FALSE, at = NULL, type = 6) {
  args <- list(x, ...)
  namedargs <- if (!is.null(attributes(args)$names))
    attributes(args)$names != ""
  else rep_len(FALSE, length(args))
  groups <- if (is.list(x))
    x
  else args[!namedargs]
  if (0L == (n <- length(groups)))
    stop("invalid first argument")
  if (length(class(groups)))
    groups <- unclass(groups)
  if (!missing(names))
    attr(groups, "names") <- names
  else {
    if (is.null(attr(groups, "names")))
      attr(groups, "names") <- 1L:n
    names <- attr(groups, "names")
  }
  cls <- sapply(groups, function(x) class(x)[1L])
  cl <- if (all(cls == cls[1L]))
    cls[1L]
  else NULL
  for (i in 1L:n) groups[i] <- list(boxplot_stats(unclass(groups[[i]]),
                                                  range, type = type))
  stats <- matrix(0, nrow = 5L, ncol = n)
  conf <- matrix(0, nrow = 2L, ncol = n)
  ng <- out <- group <- numeric(0L)
  ct <- 1
  for (i in groups) {
    stats[, ct] <- i$stats
    conf[, ct] <- i$conf
    ng <- c(ng, i$n)
    if ((lo <- length(i$out))) {
      out <- c(out, i$out)
      group <- c(group, rep.int(ct, lo))
    }
    ct <- ct + 1
  }
  if (length(cl) && cl != "numeric")
    oldClass(stats) <- cl
  z <- list(stats = stats, n = ng, conf = conf, out = out,
            group = group, names = names)
  if (plot) {
    if (is.null(pars$boxfill) && is.null(args$boxfill))
      pars$boxfill <- col
    do.call("bxp", c(list(z, notch = notch, width = width,
                          varwidth = varwidth, log = log, border = border,
                          pars = pars, outline = outline,
                          horizontal = horizontal, add = add, at = at),
                     args[namedargs]))
    invisible(z)
  }
  else z
}

#' @export
box_plot.matrix <- function (x, use.cols = TRUE, ...) {
  groups <- if (use.cols) {
    split(c(x), rep.int(1L:ncol(x), rep.int(nrow(x), ncol(x))))
  }
  else split(c(x), seq(nrow(x)))
  if (length(nam <- dimnames(x)[[1 + use.cols]]))
    names(groups) <- nam
  invisible(box_plot(groups, ...))
}

#' @export
box_plot.formula <- function (formula, data = NULL, ..., subset,
                              na.action = NULL) {
  if (missing(formula) || (length(formula) != 3L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m$... <- NULL
  m$na.action <- na.action
  m[[1L]] <- quote(stats::model.frame)
  mf <- eval(m, parent.frame())
  response <- attr(attr(mf, "terms"), "response")
  box_plot(split(mf[[response]], mf[-response]), ...)
}

#' Box Plot Statistics
#'
#' A copy of \code{\link[grDevices]{boxplot.stats}} that adds the argument
#' \code{type} to be passed to \code{\link[stats]{quantile}} when calculating
#' sample quantiles.
#' @param x a numeric vector for which the boxplot will be constructed
#'   (\code{\link{NA}}s and \code{\link{NaN}}s are allowed and omitted).
#' @param coef this determines how far the plot whiskers extend out from the
#'   box. If coef is positive, the whiskers extend to the most extreme data
#'   point which is no more than coef times the length of the box away from
#'   the box. A value of zero causes the whiskers to extend to the data
#'   extremes (and no outliers be returned).
#' @param do.conf,do.out logicals; if FALSE, the conf or out component
#'   respectively will be empty in the result.
#' @param type Argument \code{type} to \code{\link[stats]{quantile}}.
#' @export
boxplot_stats <- function (x, coef = 1.5, do.conf = TRUE, do.out = TRUE,
                           type = 6){
  if (coef < 0)
    stop("'coef' must not be negative")
  nna <- !is.na(x)
  n <- sum(nna)
  stats <- five_number(x, na.rm = TRUE, type = type)
  iqr <- diff(stats[c(2, 4)])
  if (coef == 0)
    do.out <- FALSE
  else {
    out <- if (!is.na(iqr)) {
      x < (stats[2L] - coef * iqr) | x > (stats[4L] + coef * iqr)
    }
    else !is.finite(x)
    if (any(out[nna], na.rm = TRUE))
      stats[c(1, 5)] <- range(x[!out], na.rm = TRUE)
  }
  conf <- if (do.conf)
    stats[3L] + c(-1.58, 1.58) * iqr / sqrt(n)
  list(stats = stats, n = n, conf = conf,
       out = if (do.out) x[out & nna] else numeric())
}

# ================================ scatter ===============================

#' Scatter plot with five number summary
#'
#' Produces a scatter plot in which the axes are labelled with
#' the respective five number summaries of the horizontal and vertical
#' axis data.
#'
#' @param x The argument \code{x} of \code{\link[graphics:plot.default]{plot}}.
#' @param y The argument \code{x} of \code{\link[graphics:plot.default]{plot}}.
#' @param ndec A numeric vector.  The numbers of decimal places to
#'   which to round the five number summaries.  If \code{ndec} is
#'   a scalar then this value is used for both axes.
#' @param type Argument \code{type} used in the call to
#'   \code{\link{five_number}} to estimate the 25\%, 50\% and 75\% quantiles.
#' @param na.rm A logical scalar.  If true, any \code{\link{NA}} and NaNs
#'   are removed before the sample quantiles are computed.
#' @param ... Further arguments to be passed to \code{plot}.
#' @return Nothing, just the plot.
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' scatter(x, y)
#' @export
scatter <- function(x, y, ndec = 1, type = 6, na.rm = FALSE, ...) {
  # Make sure that ndec has length 2
  ndec <- rep_len(ndec, 2)
  # Calculate five number summaries of x and y.
  fx <- five_number(x, type = type, na.rm = na.rm)
  fy <- five_number(y, type = type, na.rm = na.rm)
  # Round the five number summaries.
  lx <- round(fx, ndec)
  ly <- round(fy, ndec)
  # Produce the plot, but with no axes.
  graphics::plot(x, y, axes = FALSE, ...)
  # Add axes with only the five number summaries labelled.
  graphics::axis(1, at = fx, labels = lx)
  graphics::axis(2, at = fy, labels = ly)
  invisible()
}


# ============================== scatter_hist =============================

#' Scatter plot with marginal histograms
#'
#' Produces a scatter plot in which the axes are supplemented by
#' histograms of the marginal horizontal and vertical axis data.
#'
#' @param x The argument \code{x} of \code{\link[graphics:plot.default]{plot}}.
#' @param y The argument \code{x} of \code{\link[graphics:plot.default]{plot}}.
#' @param xbreaks A numeric vector.  Optional argument \code{breaks}
#'   to \code{\link[graphics]{hist}} when plotting the histogram
#'   on the horizontal axis.
#' @param ybreaks A numeric vector.  Optional argument \code{breaks}
#'   to \code{\link[graphics]{hist}} when plotting the histogram
#'   on the vertical axis.
#' @param ... Further arguments to be passed to \code{plot}.
#' @return Nothing, just the plot.
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' scatter_hist(x, y)
#' @export
scatter_hist <- function(x, y, xbreaks = NULL, ybreaks = NULL, ...) {
  # save default, for resetting...
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))
  # Extract any arguments supplied in ....
  user_args <- list(...)
  # If the user wants a log-scale on an axis make a transformation of
  # the data so that the histogram matches the scatterplot.
  if (!is.null(user_args$log)) {
    if (user_args$log == "x") {
      xh <- log(x)
      yh <- y
    }
    if (user_args$log == "y") {
      yh <- log(y)
      xh <- x
    }
    if (user_args$log == "xy") {
      xh <- log(x)
      yh <- log(y)
    }
  } else {
    xh <- x
    yh <- y
  }
  # create a layout to contain the main scatter plot and the histograms.
  nf <- graphics::layout(matrix(c(2, 0, 1, 3), 2, 2, byrow = TRUE), c(3, 1),
                         c(1, 3), TRUE)
  graphics::par(mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 0, 0))
  # Produce the scatter plot.
  graphics::plot(x, y, ...)
  graphics::par(mar = c(0, 3, 1, 1))
  # Produce the histogram for the horizontal axis.
  if (is.null(xbreaks)) {
    xhist <- graphics::hist(xh, plot = FALSE)
  } else {
    xhist <- graphics::hist(xh, breaks = xbreaks, plot = FALSE)
  }
  graphics::barplot(xhist$count, axes = FALSE, space = 0)
  graphics::par(mar = c(3, 0, 1, 1))
  # Produce the histogram for the vertical axis.
  if (is.null(ybreaks)) {
    yhist <- graphics::hist(yh, plot = FALSE)
  } else {
    yhist <- graphics::hist(yh, breaks = ybreaks, plot = FALSE)
  }
  graphics::barplot(yhist$count, axes = FALSE, space = 0, horiz = TRUE)
  invisible()
}


# ================================== qqexp ====================================

#' Expnential Quantile-Quantile plots
#'
#' Produces a QQ plot to compare ordered sample data to corresponding
#' quantiles of an exponential distribution fitted to these data.
#'
#' @details The rate parameter \eqn{\lambda} of the exponential distribution
#'   is estimated using \code{1 / mean(y)}.  The ordered sample data are
#'   plotted against quantiles of this fitted exponential distribution.
#'   Specifically, the \eqn{i}th smallest sample observation is plotted
#'   against the \eqn{100 i / (n + 1)\%} theoretical exponential quantile,
#'   where \eqn{n} is the sample size. The plot is constrained to be square.
#' @param y Sample data
#' @param ... Optional \code{\link[graphics:par]{graphical parameters}}
#'   passed to \code{\link[graphics:plot.default]{plot}}, such as \code{pch},
#'   \code{lty} and \code{lwd}, to control the appearance of the plot.
#' @param main A character scalar. The title (if any) to give the plot.
#' @param line Determines whether or not a line of equality is superimposed on
#'   the plot.  If a line is required then must be a list, which can contain
#'   \code{\link[graphics:par]{graphical parameters}}
#'   passed to \code{\link[graphics]{abline}}, such as \code{col}, \code{lty}
#'   and \code{lwd} to control the appearance of the line. If \code{line} is
#'   not a list, for example, \code{line = 0}, then no line is superimposed.
#' @param envelopes Determines whether or not simulation envelopes should be
#'   added to the plot.  If \code{envelopes = FALSE} then no envelopes are
#'   added. If \code{envelopes} is a positive integer (a common choice is 19)
#'   then simulation envelopes based on this many simulated datasets are added.
#'   The limits of of the envelopes are indicated using short horizontal lines.
#' @return Nothing, just the plot.
#' @seealso \code{\link[stats]{qqnorm}} for a normal QQ plot.
#' @examples
#' # Australian Birth Times
#' # Calculate the waiting times until each birth
#' waits <- diff(c(0, aussie_births[, "time"]))
#' qqexp(waits, pch = 16, line = list(lty = 2, col = "blue", lwd = 2))
#' @export
qqexp <- function(y, ..., main = "Exponential QQ plot",
                  line = list(col = "black", lty = 1, lwd = 1),
                  envelopes = FALSE) {
  # save default, for resetting...
  old_par <- graphics::par(pty = "s", no.readonly = TRUE)
  on.exit(graphics::par(old_par))
  # Extract any arguments supplied in ....
  user_args <- list(...)
  # Sample size
  n <- length(y)
  # Estimate lambda
  lambdahat <- 1 / mean(y)
  # Exponential quantiles
  exp_quantiles <- stats::qexp((1:n) / (n + 1), rate = lambdahat)
  add_env <- FALSE
  # If envelopes are required then perform the simulation
  if (is.numeric(envelopes)) {
    if (!is.positiveinteger(envelopes)) {
      stop("''envelopes'' must be a positive integer")
    }
    # Create a n by envelopes matrix of simulated values
    sim_values <- matrix(stats::rexp(envelopes * n, rate = lambdahat),
                         ncol = envelopes, nrow = n)
    # Sort each column, i.e., each of the envelopes datasets
    sorted_sim_values <- apply(sim_values, 2, sort)
    # Find the minimum and maximum values for each row
    lower <- apply(sorted_sim_values, 1, min)
    upper <- apply(sorted_sim_values, 1, max)
    add_env <- TRUE
  } else {
    upper <- NULL
  }
  # Produce the plot: ordered data vs exponential quantiles
  my_plot_fn <- function(x, y, ..., xlab, ylab, xlim, ylim) {
    graphics::plot(x, y, ..., xlab = xlab, ylab = ylab, xlim = xlim,
                   ylim = ylim)
    # Add a line of equality
    if (is.list(line)) {
      for_abline <- c(line, a = 0, b = 1)
      do.call(graphics::abline, for_abline)
    }
  }
  xlab <- paste0("Theoretical exponential quantiles")
  ylab <- "Sample quantiles"
  max_value <- max(exp_quantiles, y, upper)
  axis_range <- c(0, max_value)
  my_plot_fn(x = exp_quantiles, y = sort(y), ..., xlab = xlab, ylab = ylab,
             xlim = axis_range, ylim = axis_range)
  # Add simulation envelopes (if required)
  if (add_env) {
    graphics::points(x = exp_quantiles, y = lower, pch = "_")
    graphics::points(x = exp_quantiles, y = upper, pch = "_")
  }
  title(main = main)
  invisible()
}
