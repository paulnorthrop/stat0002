
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.org/paulnorthrop/stat0002.svg?branch=master)](https://travis-ci.org/paulnorthrop/stat0002)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/paulnorthrop/stat0002?branch=master&svg=true)](https://ci.appveyor.com/project/paulnorthrop/stat0002)

# stat0002 <img src="standalone.png" align="right" />

## Introduction to Probability and Statistics

### What does stat0002 do?

The `stat0002` package provides R code, datasets and vignettes
(tutorials demonstrating how to use R code) to help students taking
STAT0002 Introduction to Probability and Statistics at University
College London (UCL) to understand the course material and to see how R
can be used to perform some of the analyses in the course.

### Installation

If you need to install R then you can get it from
[CRAN](https://cran.r-project.org/).

If you use a **Mac** then please read the notes at [R Mac OS
X](https://cran.r-project.org/bin/macosx/), in particular the 3
paragraphs under **Latest release:**. You need all of the software
described here.

If you would like to use RStudio (a free user-friendly front-end to R)
then you can get it from the [Download RStudio
page](https://www.rstudio.com/products/rstudio/download/). Find **All
Installers** near the bottom of the page and choose the installer for
your operating system.

To install `stat0002` type (you can copy and paste) the following at the
R Console command prompt `>`.

``` r
install.packages("remotes")
install.packages(c("plotrix", "rpanel", "rust", "smovie", "tkrplot", "MASS", "knitr", "distributions3", "SuppDists"), 
                 dependencies = "Depends")
```

You only need to do this once. Then install `stat0002` (or reinstall it
to get the latest version) using

``` r
remotes::install_github("paulnorthrop/stat0002", dependencies = "Depends", build_vignettes = TRUE)
```

### Getting started

Then type

``` r
library(stat0002)
?stat0002
```

to open the main help page, which contains links to vignettes, datasets
and movies. The Index at the bottom links to help files for functions.
If `?stat0002` doesn’t work then close RStudio, reopen it and try again.

However, perhaps the most convenient source of this information is the
[stat0002 home page on
GitHub](https://paulnorthrop.github.io/stat0002/), which has help files
under **Reference** and vignettes under **Articles**.

You will find that some vignettes contain ideas that we have not yet
covered in lectures. If you want to try to understand these ideas before
we cover them then please use the links to further information that have
been provided.

If you have any questions about this package please ask them via the
[STAT0002 Moodle Discussion
Forum](https://moodle.ucl.ac.uk/mod/hsuforum/view.php?id=866683).

### Software required for the movies

The movies are produced using the
[`rpanel`](https://cran.r-project.org/package=rpanel) package, which
requires the Tcl extension
[`BWidget`](https://sourceforge.net/projects/tcllib/files/BWidget/).
`BWidget` is included in the R installers for Windows and macOS. For
other platforms please see [Section 1.1.7 of Writing R
Extensions](https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Non_002dR-scripts-in-packages)
for installation advice.
