#' Sampling distribution of the correlation coefficient movie
#'
#' @inherit smovie::correlation
#' @examples
#' corr_sim_movie(rho = 0.8)
#' corr_sim_movie(n = 10)
#' @export
corr_sim_movie <- function(n = 30, rho = 0, panel_plot = TRUE, hscale = NA,
                           vscale = hscale, delta_n = 1, delta_rho = 0.1, ...) {
  # Check for tcltk but do not throw an error.
  # This is part of a hack to enable a mac build using CRAN's macOS builder
  if (!requireNamespace("tcltk", quietly = TRUE)) {
    cat("Package \"tcltk\" must be installed to use this function. \n")
    cat("You are probably using an Apple Mac. \n")
    cat("Reinstall R using a *default*, not custom, installation. \n")
    cat("See https://cran.r-project.org/bin/macosx/. \n")
    return(invisible())
  }
  smovie::correlation(n = 30, rho = 0, panel_plot = TRUE, hscale = NA,
                      vscale = hscale, delta_n = 1, delta_rho = 0.1, ...)
}
