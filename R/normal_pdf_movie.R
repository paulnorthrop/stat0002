# ============================== normal_pdf_movie =============================

#' Normal p.d.f. movie
#'
#' A movie to show how the probability density function (p.d.f.) of the normal
#' distribution varies as the mean and/or variance of the distribution
#' are changed. For more continuous distributions, including the exponential
#' and uniform distributions, use \code{smovie::movies()} and click on the
#' \strong{Continuous} menu.
#' This is based on the \code{\link[smovie]{continuous}} function.
#' (If you have not installed the \code{smovie} package then use
#' \code{install.packages("smovie")} to install it.)
#'
#' @param starting_mean A numeric scalar.  The value of the mean used
#'   to produce the first plot of the p.d.f.
#' @param starting_var A positive numeric scalar.  The value of the variance
#'   used to produce the first plot of the p.d.f.
#' @param delta_mean A numeric scalar.  The amount by which the value of the
#'   mean is increased/decreased after one click of the +/- button for the
#'   mean.
#' @param delta_var A numeric scalar.  The amount by which the value of the
#'   variance is increased/decreased after one click of the +/- button for the
#'   variance.
#' @details The values of the mean and/or variance can be changed using the
#'   +/- buttons in the panel.
#'   For the purposes of this movie, the variance cannot be reduced below a
#'   value of 0.1.
#' @return Nothing is returned, only the animation is produced.
#' @seealso \code{\link{stat0002movies}}: general information about the movies.
#' @seealso \code{\link{normal_areas_movie}}: normal central probability areas
#'   movie.
#' @examples
#' normal_pdf_movie()
#' normal_pdf_movie(delta_var = 1)
#' @export
normal_pdf_movie <- function(starting_mean = 0, starting_var = 1,
                             delta_mean = 0.1, delta_var = 0.1) {
  # Check for tcltk but do not throw an error.
  # This is part of a hack to enable a mac build using CRAN's macOS builder
  if (!requireNamespace("tcltk", quietly = TRUE)) {
    cat("Package \"tcltk\" must be installed to use this function. \n")
    cat("You are probably using an Apple Mac. \n")
    cat("Reinstall R using a *default*, not custom, installation. \n")
    cat("See https://cran.r-project.org/bin/macosx/. \n")
    return(invisible())
  }
  if (starting_var <= 0) {
    stop("starting_var must be positive")
  }
  normal.panel <- rpanel::rp.control("normal parameters", mu = starting_mean,
                                     sigma2 = starting_var)
  plot_pdf_normal(list(mu = starting_mean, sigma2 = starting_var))
  mu <- starting_mean
  sigma2 <- starting_var
  # Create buttons for movie
  rpanel::rp.doublebutton(normal.panel, mu, delta_mean, range=c(-1e6, 1e6),
                  repeatinterval = 20, title = "mean:",
                  action = plot_pdf_normal)
  rpanel::rp.doublebutton(normal.panel, sigma2, delta_var, range=c(0.1, 1e6),
                  repeatinterval = 20, title = "variance:",
                  action = plot_pdf_normal)
  return(invisible())
}

# Function to be called by normal_pdf_movie().

plot_pdf_normal <- function(panel) {
  with(panel, {
    sigma <- sqrt(sigma2)
    ytop <- stats::dnorm(0, mean = 0, sd =sqrt(0.1))
    from <- min(stats::qnorm(0.001, mean = mu, sd = sigma), -10)
    to <- max(stats::qnorm(0.999, mean = mu, sd = sigma), 10)
    graphics::curve(stats::dnorm(x,mean = mu, sd = sigma), from = from, to = to,
                    n = 500, bty = "l", axes = FALSE, ylab = "f(z)",
                    ylim = c(0, ytop), xlab = "z", las = 1, xpd = TRUE,
                    lwd = 2)
    graphics::axis(1, pos = 0)
    graphics::axis(2, pos = from, las = 1)
    mutext <- as.character(round(mu, 3))
    sigtext <- as.character(round(sigma2, 3))
    graphics::title(paste("normal distribution:  mean =", mutext, ", variance =", sigtext))
  })
  panel
}
