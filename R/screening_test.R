# ============================ screening_test() ===============================

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
#'   the randomly chosen person tests positive then the sensitivity is
#'   \eqn{P(+ \mid D)}.
#' @param specificity A numeric scalar.  The conditional probability that a
#'   person who does not have the disease tests negative.  If \eqn{-} is the
#'   event that the randomly chosen person test negative then the sensitivity
#'   is \eqn{P(- \mid {\rm not}D)}{P(- | notD)}.
#' @details The required probabilities are calculated using the **law of total
#'   probability**
#'   \deqn{P(+) = P(+ \mid D) P(D) + P(+ \mid {\rm not}D) P({\rm not}D)}{%
#'     P(+) = P(+ | D) P(D) + P(+ | notD) P(notD)}
#'   and **Bayes' theorem**
#'   \deqn{P(D \mid +) = \frac{P(+ \mid D) P(D)}{P(+)}}{%
#'     P(D | +) = P(+ | D) P(D) / P(+)}
#'   \deqn{P({\rm not}D \mid -) = \frac{P(- \mid {\rm not}D) P({\rm not}D)}{P(-)}.}{%
#'     P(notD | -) = P(- | notD) P(notD) / P(-).}
#' @return A list containing the following components
#' * `pp` The probability \eqn{P(+)} that the person will test positive.
#' * `ppv` The positive predictive value. The conditional probability
#'   \eqn{P(D \mid +)} if the person tests positive then they has the disease.
#' * `npv` The negative predictive value. The conditional probability
#'   \eqn{P({\rm not}D \mid -)}{P(notD | -)} if the person tests negative
#'   then they do not have the disease.
#' * `prior`,`sensitivity`,`specificity` The input values of `prior`,
#'   `sensitivity` and `specificity`.
#' @examples
#' screening_test(prior = 0.1, sensitivity = 0.9, specificity = 0.9)
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
  res <- list(pp = pp, ppv = ppv, npv = npv, prior = prior,
              sensitivity = sensitivity, specificity = specificity)
  class(res) <- "screening_test"
  return(res)
}

# ============================ print.screening() ==============================

#' Prints a \code{screening_test} object
#'
#' \code{print} method for class "screening_test".
#'
#' @param x an object of class "screening_test", a result of a call to
#'   \code{\link{screening_test}}.
#' @param digits An integer.  The number of significant digits to include in
#'   the printed probabilities.  Passed to \code{\link{format}}.
#' @param ... Additional arguments.  None used at present.
#' @return Nothing is returned.
#' @examples
#' screening_test(0.1, 0.9, 0.9)
#' @seealso \code{\link{screening_test}}.
#' @export
print.screening_test <- function(x, digits = max(3L, getOption("digits") - 3L),
                                                 ...) {
  if (!inherits(x, "screening_test")) {
    stop("use only with \"screening_test\" objects")
  }
  inputs <- c("P(D)" = x$prior, "P(+ | D)" = x$sensitivity,
              "P(- | notD)" = x$specificity)
  cat("Prevalence, sensitivity, specificity:\n")
  print.default(format(inputs, digits = digits), print.gap = 2L,
                quote = FALSE)
  outputs <- c("P(+)" = x$pp, "P(D | +)" = x$ppv, "P(notD | -)" = x$npv)
  cat("P(positive test), positive and negative predictive values:\n")
  print.default(format(outputs, digits = digits), print.gap = 2L,
                quote = FALSE)
  return(invisible(x))
}
