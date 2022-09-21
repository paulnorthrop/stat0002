
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/paulnorthrop/stat0002?branch=master&svg=true)](https://ci.appveyor.com/project/paulnorthrop/stat0002)

# stat0002 <img src="standalone.png" align="right" />

## Introduction to Probability and Statistics

### What does stat0002 do?

The `stat0002` package provides R code, datasets, ‘movies’ (interactive
plots) and articles (tutorials demonstrating how to use R code) to help
students taking STAT0002 Introduction to Probability and Statistics at
University College London (UCL) to understand the course material and to
see how R can be used to perform some of the analyses in the course.

Currently, it is not compulsory for you to use R when studying for
STAT0002. However, it is strongly recommended that you use R and make
use of the content of the `stat0002` package. Being able to use a
statistical computing package like R is essential for performing
statistical analyses and many students taking STAT0002 will need to use
R in other modules.

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

to open the main help page, which contains links to articles with
tutorials about R code, datasets and movies. The Index at the bottom
links to help files for individual functions and datasets. If
`?stat0002` doesn’t work then close RStudio, reopen it and try again.

However, perhaps the most convenient source of this information is the
[stat0002 home page on
GitHub](https://paulnorthrop.github.io/stat0002/), which has help files
under **Reference** and tutorials under **Articles** (direct link:
[Articles](https://paulnorthrop.github.io/stat0002/articles/)).

### Suggestions about how to use this resource

-   Use the tutorials to see how some of the content of STAT0002 is
    produced. There are questions to consider in some of these
    tutorials.
-   Look at the example datasets when they arise during the module. Play
    with them in R.
-   Run the ‘movies’ shown in STAT0002 workshops and videos for
    yourself.
-   Look at the code in some of the R functions. For example, if you
    type `scatter` in the R console and press return then you will see
    the content of the function `scatter`. Use `?scatter` to find out
    what the function does and look at the code tosee how it does it.

You will find that some tutorials contain ideas that we have not yet
covered in lectures and occasionally things that are beyond the scope of
STAT0002. If you want to try to understand ideas before we cover them
then please use the links to further information that have been
provided.
