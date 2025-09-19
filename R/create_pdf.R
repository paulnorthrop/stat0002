#' Create PDF output
#'
#' Functions to create PDF files containing R output and/or Microsoft Word or
#' HTML files from which PDF files can be produced.
#'
#' @param x A character vector containing the names (\strong{no extension}) of
#'   the \code{.Rmd} files to convert if they are in the current working
#'   directory, or paths to the files, either absolute or relative to the
#'   current working directory, e.g., \code{DIRECTORY/file1}.  The output
#'   files are created in the same directory as their respective \code{.Rmd}
#'   file.  If \code{x} is missing then an output file is created from each of
#'   the \code{.Rmd} files in the current working directory, given by
#'   \code{getwd()}.
#' @param pdf A logical scalar, either \code{TRUE} or \code{FALSE}.
#'   If \code{pdf = FALSE} then no PDF files are created. If \code{pdf = TRUE}
#'   then what happens depends on whether \code{word} or \code{html} is used.
#'
#' \itemize{
#'   \item \code{word}: if the Operating System is \code{"windows"}, that is,
#'     \code{.Platform$OS.type == "windows"} and the `OfficeToPDF` software has
#'     been installed (see \code{\link[accessr]{install_otp}}) then a PDF file
#'     is created from each Word file. Otherwise, no PDF files are created.
#'   \item \code{html}: each html file is printed to a PDF file using
#'   \code{\link[pagedown]{chrome_print}}. Google Chrome (or an alternative
#'   browser specified in \code{pdf_args} by the \code{browser} argument to
#'   \code{\link[pagedown]{chrome_print}}) must be installed prior to use of
#'   this option. An error message like
#'   \code{Error in servr::random_port(NULL) : Cannot find an available TCP port}
#'   means that the \code{random_port} function in the \code{servr}
#'   package could not find an internet connection that Chrome considers
#'   secure.  Perhaps you are using a coffee shop's wifi.
#'   }
#' @param zip A logical scalar. The argument \code{zip} to
#'   \code{\link[accessr]{rmd2word}} or \code{\link[accessr]{rmd2html}}.
#'   If \code{zip = TRUE} then zip archives are produced containing the
#'   files created.
#' @param ... Further arguments to be passed to \code{\link[accessr]{rmd2word}}
#'   (for the function \code{word}) or \code{\link[accessr]{rmd2html}} (for
#'   the function \code{html}). Explain \code{zip}.
#' @details The functions \code{\link[accessr]{rmd2word}} (for \code{word})
#'   or \code{\link[accessr]{rmd2html}} (for \code{html}) from the
#'   \code{\link[accessr]{accessr-package}} are used.
#' @return In addition to creating the required files, a list containing
#'   information about the files created. See the \strong{Return} section of
#'   \code{\link[accessr]{rmd2word}} or \code{\link[accessr]{rmd2html}} for
#'   details.
#' @note The \code{pandoc} software version 1.14 or higher is required, that
#'   is, \code{rmarkdown::pandoc_available("1.14")} must return \code{TRUE}.
#'   \code{pandoc} should be installed automatically if you are using RStudio.
#' @seealso \code{\link[accessr]{rmd2word}}, \code{\link[accessr]{rmd2html}},
#'   \code{\link[accessr]{install_otp}}.
#' @examples
#'
#' # We copy example.Rmd to the current working directory.
#' # Then we call word() and html().
#'
#' eg <- system.file(package = "stat0002", "examples", "example.Rmd")
#' file.copy(eg, getwd(), overwrite = TRUE)
#'
#' # Create a Word file only
#' word("example", pdf = FALSE)
#'
#' # Create an HTML file only
#' html("example", pdf = FALSE)
#'
#' # We repeat this passing pdf = TRUE to create PDF files.
#' # This is not run when the package is tested because it will not work on
#' # all operating systems.
#' \dontrun{
#'
#'   # Create a Word file and a PDF file
#'   word("example", pdf = TRUE)
#'
#'   # Create an HTML file and a PDF file
#'   html("example", pdf = TRUE)
#' }
#' @name create_pdf
NULL
## NULL

#' @rdname create_pdf
#' @export
word <- function(x, pdf = isTRUE(.Platform$OS.type == "windows"), zip = FALSE,
                 ...) {
  access::rmd2word(x, pdf = pdf, zip = zip, ...)
}

#' @rdname create_pdf
#' @export
html <- function(x, pdf = TRUE, zip = FALSE, ...) {
  access::rmd2html(x, pdf = pdf, zip = zip, ...)
}
