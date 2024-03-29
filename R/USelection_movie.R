# =========================== USelection_movie ===========================

#' US 2000 Presidential Election Movie: straightening scatter plots
#'
#' A movie to illustrate the effects of the transformation of variable(s)
#' on the appearance of a scatter plot, using the 2000 U.S. Presidential
#' Election data from Florida.
#'
#' @param x,y Numeric vectors of the same length.  Pairs of values to plot
#'   using a scatter plot.  The values in \code{x} will be plotted on the
#'   horizontal axis, the values in \code{y} on the vertical axis.
#' @param delta_power A numeric scalar.  The amount by which the powers of
#'   \code{x} and \code{y} increase/decrease after one click of a button
#'   in the parameter window.
#' @param pos A numeric integer.  Used in calls to \code{\link{assign}}
#'   to make information available across successive frames of a movie.
#'   By default, uses the current environment.
#' @param envir An alternative way (to \code{pos}) of specifying the
#'   environment. See \code{\link{environment}}.
#' @param ... Further arguments to be passed to
#'   \code{\link[graphics:plot.default]{plot}}.
#' @details \code{scatterplot_movie} produces a scatter plot of the input
#'   variables \code{x} and \code{y} which can then be animated by
#'   transforming \code{x} and/or \code{y} using power transformations.
#'   [In fact a
#'   \href{https://en.wikipedia.org/wiki/Power_transform#Box.E2.80.93Cox_transformation}{Box-Cox transformation}
#'   is used.]
#'   The power of \code{x} and \code{y} is chosen using a parameter windows
#'   containing buttons labelled + and -.
#'   Clicking + increases the power by \code{delta_power} and
#'   clicking - decreases the power by \code{delta_power}.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{stat0002movies}}: general information about the movies.
#' @seealso \code{\link{USelection}}: description of the 2000 U.S.
#'   Presidential Election data from Florida.
#' @examples
#' # Proportion of votes gained by Buchanan
#' pbuch <- 100 * USelection$buch / USelection$tvot
#'
#' # Produce plot
#' scatterplot_movie(x = USelection$npop, y = pbuch)
#' # Change the plotting character
#' scatterplot_movie(x = USelection$npop, y = pbuch, pch = 16)
#'
#' # Identify Palm Beach using a different plotting character
#' county_name <- USelection[, "co_names"]
#' pb <- which(county_name == "PalmBeach")
#' my_pch <- rep(16, length(county_name))
#' my_pch[pb] <- 4
#' scatterplot_movie(x = USelection$npop, y = pbuch, pch = my_pch)
#' @export
scatterplot_movie <- function(x, y, delta_power = 0.1, pos = 1,
                              envir = as.environment(pos), ...) {
  # Check for tcltk but do not throw an error.
  # This is part of a hack to enable a mac build using CRAN's macOS builder
  if (!requireNamespace("tcltk", quietly = TRUE)) {
    cat("Package \"tcltk\" must be installed to use this function. \n")
    cat("You are probably using an Apple Mac. \n")
    cat("Reinstall R using a *default*, not custom, installation. \n")
    cat("See https://cran.r-project.org/bin/macosx/. \n")
    return(invisible())
  }
  # Assign them to an environment so that they can be accessed inside
  # shuttle_sim_rpanel_plot()
  assign("x", x, envir = envir)
  assign("y", y, envir = envir)
  # Pass arguments for plot() supplied by the user
  user_args <- list(...)
  assign("user_args", user_args, envir = envir)
  scatterplot_panel <- rpanel::rp.control("parameters")
  c_x <- 1
  c_y <- 1
  rpanel::rp.doublebutton(scatterplot_panel, c_y, delta_power,
                          range=c(-50, 50), initval = 1, title = "power of y:",
                          action = scatterplot_movie_plot)
  rpanel::rp.doublebutton(scatterplot_panel, c_x, delta_power,
                          range=c(-50, 50), initval = 1, title = "power of x:",
                          action = scatterplot_movie_plot)
  rpanel::rp.do(scatterplot_panel, scatterplot_movie_plot)
  invisible()
}

# Function to be called by scatterplot_movie().

scatterplot_movie_plot <- function(panel){
  with(panel, {
    x_trans <- bc(x, c_x)
    y_trans <- bc(y, c_y)
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par))
    graphics::par(las = 1)
    for_plot <- c(list(x = x_trans, y = y_trans, ylab = "", xlab = "",
                       axes = FALSE), user_args)
    do.call(graphics::plot, for_plot)
    box()
    if (c_x == 0) {
      mtext("ln(x)", side = 1, line = 1.5)
    } else {
      mtext(bquote(frac(x^.(c_x) - 1, .(c_x))), side = 1, line = 2.5)
    }
    if (c_y == 0) {
      mtext("ln(y)", side = 2, line = 1.2)
    } else {
      mtext(bquote(frac(y^.(c_y) - 1, .(c_y))), side = 2, line = 0.8)
    }
  })
  invisible(panel)
}

# Box-Cox transformation

bc <- function(x, c){
  if (c == 0) {
    y <- log(x)
  } else {
    y <- (x ^ c - 1) / c
  }
  return(y)
}

