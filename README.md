
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

Most of the functions return not only the resulting data, but attach the
*tidyverse* code used to generate it. This is useful for GUIs that
display code history (e.g., *iNZight*) or when learning to code.

### Data import

Importing data is done using the `smart_read()` function, which can read
CSV, Excel, Stata, SAS, RData, and a few other formats based on the file
extension.

``` r
data <- smart_read(system.file("extdata/cas500.xls", package = "iNZightTools"))
#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

#> Warning in mask$eval_all_mutate(quo): NAs introduced by coercion
str(data)
#> tibble [500 × 10] (S3: tbl_df/tbl/data.frame)
#>  $ cellsource: Factor w/ 5 levels "job","NA","other",..: 5 4 4 5 5 4 4 5 4 3 ...
#>  $ rightfoot : num [1:500] 20 25 21 20 23 19 23 35 22 30 ...
#>  $ travel    : Factor w/ 6 levels "bike","bus","motor",..: 6 4 3 6 4 3 3 3 3 6 ...
#>  $ getlunch  : Factor w/ 7 levels "dairy","friend",..: 3 2 3 3 3 3 3 7 3 7 ...
#>  $ height    : num [1:500] 152 153 137 115 165 137 164 150 150 123 ...
#>  $ gender    : Factor w/ 2 levels "female","male": 2 1 2 2 1 1 1 1 1 2 ...
#>  $ age       : num [1:500] 12 11 10 9 14 11 12 15 12 14 ...
#>  $ year      : num [1:500] 7 6 6 5 10 7 8 11 8 9 ...
#>  $ armspan   : num [1:500] 150 152 132 130 160 50 164 100 152 23 ...
#>  $ cellcost  : num [1:500] 30 50 55 60 20 50 10 20 10 0 ...
#>  - attr(*, "code")= chr "readxl::read_excel(\"/home/tom/R/x86_64-pc-linux-gnu-library/4.1/iNZightTools/extdata/cas500.xls\") %>% dplyr::"| __truncated__
tidy_all_code(code(data))
#>  [1] "readxl::read_excel(\"/home/tom/R/x86_64-pc-linux-gnu-library/4.1/iNZightTools/extdata/cas500.xls\") %>%"
#>  [2] "    dplyr::mutate_at("                                                                                  
#>  [3] "        c("                                                                                             
#>  [4] "            \"cellsource\","                                                                            
#>  [5] "            \"travel\","                                                                                
#>  [6] "            \"getlunch\","                                                                              
#>  [7] "            \"gender\""                                                                                 
#>  [8] "        ),"                                                                                             
#>  [9] "        as.factor"                                                                                      
#> [10] "    ) %>%"                                                                                              
#> [11] "    dplyr::mutate_at("                                                                                  
#> [12] "        c("                                                                                             
#> [13] "            \"rightfoot\","                                                                             
#> [14] "            \"height\","                                                                                
#> [15] "            \"age\","                                                                                   
#> [16] "            \"armspan\","                                                                               
#> [17] "            \"cellcost\""                                                                               
#> [18] "        ),"                                                                                             
#> [19] "        as.numeric"                                                                                     
#> [20] "    )"
```

1.  with others being modified in time
