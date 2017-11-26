## The idea of this script is to use it as a 'playground' while writing R package functions
#
# install.packages('devtools')  ## -- install devtools package if you haven't already

## Ideally, you won't import any other libraries this way ...
library(devtools)
library(magrittr)

## use the census at school data (this is our "primary example")
# install_github('iNZightVIT/FutureLearnData')  ## -- install this once, directly from github
data('census.at.school.500', package = 'FutureLearnData')
dat <- census.at.school.500 ## - give it a shorter name

# setwd('/path/to/iNZightTools')  ## -- working directory needs to be `iNZightTools` (R studio might do this for you?)

## load package
# - effectively: install.packages('/path/to/iNZightTools'); library(iNZightTools)
load_all()  ## Rstudio has a shortcut for this, probably.

## development code ...

## e.g., one function already written in tidyverse is the `convert to categorical` function:
dat <- dat %>% numToCat("age")
cat(sep="\n", code(dat))


### DATASET > FILTER LEVELS OF A CATEGORICAL
# filter travel = walk, bike
var <- "travel"
levels <- c("bike", "walk")

dat.filtered <- dat %>% 
  dplyr::filter(travel %in% levels) %>% 
  dplyr::mutate(travel = factor(travel, levels = levels))

filterLevels <- function(.data, var, levels) {
    ## ... code ...
}

## example 
# dat.filtered <- filterLevels(dat, "travel", c("bike", "walk"))
head(dat.filtered)
all(levels(dat.filtered$travel) == c("bike", "walk"))






## I've also started some tests, which can be run using
test()

