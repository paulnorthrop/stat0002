#' Sampling distribution of the correlation coefficient movie
#'
#' @inherit smovie::correlation
#' @examples
#' corr_sim_movie(rho = 0.8)
#' corr_sim_movie(n = 10)
corr_sim_movie <- function(n = 30, rho = 0, panel_plot = TRUE, hscale = NA,
                           vscale = hscale, delta_n = 1, delta_rho = 0.1, ...) {
  smovie::correlation(n = 30, rho = 0, panel_plot = TRUE, hscale = NA,
                      vscale = hscale, delta_n = 1, delta_rho = 0.1, ...)
}
