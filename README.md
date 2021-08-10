
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iNZightTools

![R-CMD-check](https://github.com/iNZightVIT/iNZightTools/workflows/R-CMD-check/badge.svg)
[![Coverage
status](https://codecov.io/gh/iNZightVIT/iNZightTools/branch/dev/graph/badge.svg)](https://codecov.io/github/iNZightVIT/iNZightTools?branch=dev)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![CRAN](https://www.r-pkg.org/badges/version/iNZightTools)](https://CRAN.R-project.org/package=iNZightTools)

Package consisting of a set of helper functions for doing data science
with *iNZight*. These functions are designed to work well with a
graphical user interface (GUI), but many\[1\] are functional for direct
use through *R*.

## Installation

The current release version is available on CRAN:

``` r
install.packages("iNZightTools")
```

The development version can be downloaded from GitHub:

``` r
remotes::install_github("iNZightVIT/iNZightTools@dev")
```

## Basic usage

The package itself doesn’t have any one specific use, but the functions
can be broken down into various workflows.

``` r
library(iNZightTools)
```

### Data import

Importing data is done using the `smart_read()` function, which can read
CSV, Excel, Stata, SAS, RData, and a few other formats based on the file
extension.

``` r
data <- smart_read('https://inzight.nz/testdata/nhanes.csv')
str(data)
```

1.  with others being modified in time
