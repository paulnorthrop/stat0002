#' Screening test calculations
#'
#' Consider a screening test for a certain disease or condition, applied to a
#' person who has a certain prior, pre-test probability of having the disease.
#' For such a person this function calculates the probabilities that: the
#' person has the disease given that they test positive; the person does not
#' have the disease given that they test negative.
#'
#' @param prior A numeric scalar.  The pre-test probability \eqn{P(D)} that the
#'   person has the disease.  In the absence of specific information this could
#'   be the prevalence of the disease in the population, that is, the
#'   proportion of people who have the disease in the general population of
#'   interest.
#' @param sensitivity A numeric scalar.  The conditional probability that a
#'   person who has the disease tests positive.  If \eqn{+} is the event that
#'   the randomly chosen person test positive then the sensitivity is
#'   \eqn{P(+ | D)}.
#' @param specificity A numeric scalar.  The conditional probability that a
#'   person who does not have the disease tests negative.  If \eqn{-} is the
#'   event that the randomly chosen person test negative then the sensitivity
#'   is \eqn{P(- | notD)}.
#' @details The probabilities are calculated using the **law of total
#'   probability** and **Bayes' theorem**.
#'   \deqn{P(+) = P(+ \mid D) P(D) + P(+ \mid {\rm not}D) P(D)}{%
#'     P(+) = P(+ | D) P(D) + P(+ | notD) P(notD)}
#' @return A list containing the following components
#' * `pp` The probability \eqn{P(+)} that a person chosen randomly from the
#'   population will test positive.
#' * `ppv` The positive predictive value. The conditional probability
#'   \eqn{P(D | +)} that a person who tests positive has the disease.
#' * `npv` The negative predictive value. The conditional probability
#'   \eqn{P(notD | -)} that a person who tests negative does not have the disease.
#' @examples
#' screening_test(0.1, 0.9, 0.9)
#' @export
#' @md
screening_test <- function(prior, sensitivity, specificity) {
  # Check inputs
  if (!is.numeric(prior) | length(prior) != 1 | prior < 0 | prior > 1) {
    stop("'prior' must be a number in [0, 1]")
  }
  if (!is.numeric(sensitivity) | length(sensitivity) != 1 | sensitivity < 0 |
      sensitivity > 1) {
    stop("'sensitivity' must be a number in [0, 1]")
  }
  if (!is.numeric(specificity) | length(specificity) != 1 | specificity < 0 |
      specificity > 1) {
    stop("'specificity' must be a number in [0, 1]")
  }
  # Use variable names that relate to Bayes' theorem
  p_d <- prior
  p_nd <- 1 - p_d
  p_p_d <- sensitivity
  p_n_d <- 1 - p_p_d
  p_n_nd <- specificity
  p_p_nd <- 1 - p_n_nd
  # Calculate properties of the test
  pp <- p_p_d * p_d + p_p_nd * p_nd
  pn <- 1 - pp
  ppv <- p_p_d * p_d / pp
  npv <- p_n_nd * p_nd / pn
  # Return a list
  return(list(pp = pp, ppv = ppv, npv = npv))
}
