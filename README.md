
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

### Software required

If you need to install R then you can get it from
[CRAN](https://cran.r-project.org/).

I recommend that you use RStudio (a **free** user-friendly front-end to
R). Install it from the [Download RStudio
page](https://www.rstudio.com/products/rstudio/download/). Find **All
Installers** near the bottom of the page and choose the installer for
your operating system.

If you use a **Mac** then the movies (interactive plots) in `stat0002`
will not work unless you have [XQuartz](https://www.xquartz.org/)
installed.

### Installation of `stat0002`

Copy and paste the following into the R console and press return.

``` r
install.packages(c("distributions3", "plotrix", "rpanel", "rust", "smovie", "SuppDists", "tkrplot"))
```

Then copy and paste **ONE** of the following into the R Console and
press return. Choose the option that is relevant to your computer.

#### Windows

``` r
install.packages("https://github.com/paulnorthrop/stat0002/raw/master/install/stat0002.zip", repos = NULL)
```

#### Apple Mac

``` r
install.packages("https://github.com/paulnorthrop/stat0002/raw/master/install/stat0002.tgz", repos = NULL)
```

#### Linux

``` r
install.packages("https://github.com/paulnorthrop/stat0002/raw/master/install/stat0002.tar.gz", repos = NULL)
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
