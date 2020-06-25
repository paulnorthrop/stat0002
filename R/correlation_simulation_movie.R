#' Sampling distribution of the correlation coefficient movie
#'
#' @inherit smovie::correlation
corr_sim_movie <- function(n = 30, rho = 0, panel_plot = TRUE, hscale = NA,
                           vscale = hscale, delta_n = 1, delta_rho = 0.1, ...) {
  smovie::correlation(n = 30, rho = 0, panel_plot = TRUE, hscale = NA,
                      vscale = hscale, delta_n = 1, delta_rho = 0.1, ...)
}
