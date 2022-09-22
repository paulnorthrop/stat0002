#' Internal stat0002 functions
#'
#' Internal stat0002 functions
#' @details
#' These functions are not intended to be called by the user.
#' @name stat0002-internal
#' @keywords internal
NULL

# ===================== To check whether x is an integer ======================

#' @keywords internal
#' @rdname stat0002-internal
is.positiveinteger <- function(x, tol = .Machine$double.eps^0.5) {
  return(abs(x - round(x)) < tol & x > 0)
}
