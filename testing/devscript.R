## The idea of this script is to use it as a 'playground' while writing R package functions
#
# install.packages('devtools')  ## -- install devtools package if you haven't already

## Ideally, you won't import any other libraries this way ...
library(devtools)
library(magrittr)

setwd("C:/Users/Owen/Documents/GitHub/iNZightTools")

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


# CHECK FOR 1 LEVEL
filtered.1LVL <- dat %>% 
  dplyr::filter(cellsource %in% c("job")) %>% 
    dplyr::mutate(cellsource = factor(cellsource, levels = c("job")))

# filtered.1LVL <- filterLevels(dat, "cellsource", c("job))
all(levels(filtered.1LVL$cellsource) == c("job"))
head(filtered.1LVL)

    # CHECK THAT OTHER CATEGORICAL COLUMNS RETAIN THEIR FACTORS
      all(c(levels(filtered.1LVL$travel) == levels(dat$travel), 
            levels(filtered.1LVL$getlunch) == levels(dat$getlunch), 
            levels(filtered.1LVL$gender) == levels(dat$gender)))

# CHECK FOR 3 LEVELS
filtered.3LVL <- dat %>% 
  dplyr::filter(getlunch %in% c("home", "tuckshop")) %>% 
    dplyr::mutate(getlunch = factor(getlunch, levels = c("home", "tuckshop", "friend")))    
all(levels(filtered.3LVL$getlunch) == c("home", "tuckshop", "friend"))

    # CHECK THAT OTHER CATEGORICAL COLUMNS RETAIN THEIR FACTORS
    all(c(levels(filtered.3LVL$cellsource) == levels(dat$cellsource), 
          levels(filtered.3LVL$travel) == levels(dat$travel), 
          levels(filtered.3LVL$gender) == levels(dat$gender)))


# DATASET -> FILTER DATASET -> NUMERIC CONDITION
###---------------------------------------------------------

filterNumeric <- function(dat, col, op, num){
  
}

# TEST FOR "<"
filtered.L <- dat %>% 
  dplyr::filter(rightfoot < 15)
# filtered.L <- filterNumeric(dat, "rightfoot", "<", 15)
all(filtered.L$rightfoot < 15)
head(filtered.L)


# TEST FOR "<="
filtered.LE <- dat %>% 
  dplyr::filter(height <= 120)
# filtered.L <- filterNumeric(dat, "height", "<=", 120)
all(filtered.LE$height <= 120)
head(filtered.LE)


# TEST FOR ">"
filtered.G <- dat %>% 
  dplyr::filter(age > 17)
# filtered.G <- filterNumeric(dat, "age", ">", 17)
all(filtered.G$age > 17)
head(filtered.G)


# TEST FOR ">="
filtered.GE <- dat %>% 
  dplyr::filter(year >= 10)
# filtered.GE <- filterNumeric(dat, "year", ">=", 10)
all(filtered.GE$year >= 10)
head(filtered.GE)


# TEST FOR "=="
filtered.E <- dat %>% 
  dplyr::filter(armspan == 140)
# filtered.E <- filterNumeric(dat, "armspan", "==", 140)
all(filtered.E$armspan == 140)
head(filtered.E)


# TEST FOR "!="
filtered.NE <- dat %>% 
  dplyr::filter(cellcost != 0)
# filtered.NE <- filterNumeric(dat, "cellcost", "!=", 0)
all(filtered.NE$cellcost != 0)
head(filtered.NE)

###---------------------------------------------------------


# DATASET -> FILTER DATASET -> ROW NUMBER
###---------------------------------------------------------

filterRow <- function(dat, rows){
  
}

filtered.ROW <- dat %>%
  dplyr::mutate(row.num = 1:nrow(dat)) %>%
    dplyr::slice(-c(1,2,3,4,6,7,8,9))
  
# filtered.ROW <- filterRow(dat, c(1,2,3,4,6,7,8,9))

# TEST CORRECT NUMBER OF ROWS
nrow(filtered.ROW) == nrow(dat) - length(c(1,2,3,4,6,7,8,9))


# TEST THAT ONLY ROW NUMBERS NOT REMOVED ARE KEPT
all(!(filtered.ROW$row.num %in% c(1,2,3,4,6,7,8,9)))
head(filtered.ROW)

###---------------------------------------------------------


# DATASET -> FILTER DATASET -> RANDOMLY
###---------------------------------------------------------

filterRandom <- function(dat, sample_size, n){
  
}

sample_size = 100
n = 4
filtered.RANDOM <- dat %>%
  dplyr::mutate(row.num = 1:nrow(dat)) %>% 
    dplyr::sample_n(sample_size * n, replace = FALSE) %>%
      dplyr::mutate(Sample.Number = rep(1:n, each=sample_size))
# filtered.RANDOM <- filterRandom(dat, sample_size, n)

# CHECK CORRECT NUMBER OF SAMPLES TAKEN
nrow(filtered.RANDOM) == sample_size * n

# CHECK ALL SAMPLES TAKEN ARE UNIQUE (ie.without replacement)
all(table(filtered.RANDOM$row.num) == 1)
head(filtered.RANDOM)


###---------------------------------------------------------


# DATASET -> SORT DATA BY VARIABLES
###---------------------------------------------------------

sortVars = function(){
  
}

# SORT 1 VAR
sorted.1VARS <- dat %>% 
  dplyr::arrange(cellsource)

head(sorted.1VARS, n= 15)

# SORT 2 VAR
sorted.2VARS <- dat %>% 
  dplyr::arrange(desc(rightfoot), travel)

head(sorted.2VARS, n = 15)

# SORT 3 VAR
sorted.3VARS <- dat %>% 
  dplyr::arrange(getlunch, desc(height), age)

head(sorted.3VARS, n = 15)

# SORT 4 VAR
sorted.4VARS <- dat %>% 
  dplyr::arrange(desc(year), armspan, desc(cellcost), gender)

head(sorted.4VARS, n = 15)

###---------------------------------------------------------


# DATASET -> AGGREGATE DATA
###---------------------------------------------------------

# not sure how to check this is correct

aggregateData = function(dat, col, summaries){
  
}

aggregated.3CATS <- dat %>% 
  dplyr::group_by(cellsource, travel, getlunch) %>%
    dplyr::summarize(rightfoot.mean = mean(rightfoot, na.rm = TRUE),
                     rightfoot.median = median(rightfoot, na.rm = TRUE),
                     rightfoot.sum = sum(rightfoot, na.rm = TRUE),
                     rightfoot.sd = sd(rightfoot, na.rm = TRUE),
                     rightfoot.iqr = IQR(rightfoot, na.rm = TRUE),
                     
                     height.mean = mean(height, na.rm = TRUE),
                     height.median = median(height, na.rm = TRUE),
                     height.sum = sum(height, na.rm = TRUE),
                     height.sd = sd(height, na.rm = TRUE),
                     height.iqr = IQR(height, na.rm = TRUE),
                     
                     age.mean = mean(age, na.rm = TRUE),
                     age.median = median(age, na.rm = TRUE),
                     age.sum = sum(age, na.rm = TRUE),
                     age.sd = sd(age, na.rm = TRUE),
                     age.iqr = IQR(age, na.rm = TRUE),
                     
                     year.mean = mean(year, na.rm = TRUE),
                     year.median = median(year, na.rm = TRUE),
                     year.sum = sum(year, na.rm = TRUE),
                     year.sd = sd(year, na.rm = TRUE),
                     year.iqr = IQR(year, na.rm = TRUE),
                     
                     armspan.mean = mean(armspan, na.rm = TRUE),
                     armspan.median = median(armspan, na.rm = TRUE),
                     armspan.sum = sum(armspan, na.rm = TRUE),
                     armspan.sd = sd(armspan, na.rm = TRUE),
                     armspan.iqr = IQR(armspan, na.rm = TRUE),
                     
                     cellcost.mean = mean(cellcost, na.rm = TRUE),
                     cellcost.median = median(cellcost, na.rm = TRUE),
                     cellcost.sum = sum(cellcost, na.rm = TRUE),
                     cellcost.sd = sd(cellcost, na.rm = TRUE),
                     cellcost.iqr = IQR(cellcost, na.rm = TRUE),
                     
                     # Only one column for count makes sense
                     count = n()
                     )
head(aggregated.3CATS)

#aggregated.3CATS = aggregateData(dat, c(cellsource, travel, getlunch), c("mean", "median", "sum", "sd", "iqr", "count"))

# MEAN CHECK
all(c(
  mean(dplyr::filter(dat, 
                     cellsource == "job", 
                     travel == "walk", 
                     getlunch == "home")$rightfoot, 
       na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  cellsource == "job", 
                  travel == "walk", 
                  getlunch == "home")$rightfoot.mean,
  
  mean(dplyr::filter(dat, 
                     cellsource == "parent", 
                     travel == "other", 
                     getlunch == "tuckshop")$rightfoot, 
       na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  cellsource == "parent", 
                  travel == "other", 
                  getlunch == "tuckshop")$rightfoot.mean,
  
  mean(dplyr::filter(dat, 
                     is.na(cellsource), 
                     travel == "motor", 
                     is.na(getlunch))$rightfoot, 
       na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  is.na(cellsource), 
                  travel == "motor", 
                  is.na(getlunch))$rightfoot.mean
  
))

# MEDIAN CHECK
all(c(
  median(dplyr::filter(dat, 
                       cellsource == "job", 
                       travel == "walk", 
                       getlunch == "home")$rightfoot, 
         na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  cellsource == "job", 
                  travel == "walk", 
                  getlunch == "home")$rightfoot.median,
  
  median(dplyr::filter(dat, 
                       cellsource == "parent", 
                       travel == "other", 
                       getlunch == "tuckshop")$rightfoot, 
         na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  cellsource == "parent", 
                  travel == "other", 
                  getlunch == "tuckshop")$rightfoot.median,
  
  median(dplyr::filter(dat, 
                       is.na(cellsource), 
                       travel == "motor", 
                       is.na(getlunch))$rightfoot, 
         na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  is.na(cellsource), 
                  travel == "motor", 
                  is.na(getlunch))$rightfoot.median
))

# SUM CHECK
all(c(
  sum(dplyr::filter(dat, 
                    cellsource == "job", 
                    travel == "walk", 
                    getlunch == "home")$rightfoot, 
      na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  cellsource == "job", 
                  travel == "walk", 
                  getlunch == "home")$rightfoot.sum,
  
  sum(dplyr::filter(dat, 
                    cellsource == "parent", 
                    travel == "other", 
                    getlunch == "tuckshop")$rightfoot, 
      na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  cellsource == "parent", 
                  travel == "other", 
                  getlunch == "tuckshop")$rightfoot.sum,
  
  sum(dplyr::filter(dat, 
                    is.na(cellsource), 
                    travel == "motor", 
                    is.na(getlunch))$rightfoot, 
      na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  is.na(cellsource), 
                  travel == "motor", 
                  is.na(getlunch))$rightfoot.sum
))

# SD CHECK
all(c(
  sd(dplyr::filter(dat, 
                   cellsource == "job", 
                   travel == "walk", 
                   getlunch == "home")$rightfoot, 
     na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  cellsource == "job", 
                  travel == "walk", 
                  getlunch == "home")$rightfoot.sd,
  
  sd(dplyr::filter(dat, 
                   cellsource == "parent", 
                   travel == "other", 
                   getlunch == "tuckshop")$rightfoot, 
     na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  cellsource == "parent", 
                  travel == "other", 
                  getlunch == "tuckshop")$rightfoot.sd,
  
  sd(dplyr::filter(dat, 
                   is.na(cellsource), 
                   travel == "motor", 
                   is.na(getlunch))$rightfoot, 
     na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  is.na(cellsource), 
                  travel == "motor", 
                  is.na(getlunch))$rightfoot.sd
))

# IQR CHECK
all(c(
  IQR(dplyr::filter(dat, 
                    cellsource == "job", 
                    travel == "walk", 
                    getlunch == "home")$rightfoot, 
      na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  cellsource == "job", 
                  travel == "walk", 
                  getlunch == "home")$rightfoot.iqr,
  
  IQR(dplyr::filter(dat, 
                    cellsource == "parent", 
                    travel == "other", 
                    getlunch == "tuckshop")$rightfoot, 
      na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  cellsource == "parent", 
                  travel == "other", 
                  getlunch == "tuckshop")$rightfoot.iqr,
  
  IQR(dplyr::filter(dat, 
                    is.na(cellsource), 
                    travel == "motor", 
                    is.na(getlunch))$rightfoot, 
      na.rm = TRUE) ==  
    dplyr::filter(aggregated.3CATS, 
                  is.na(cellsource), 
                  travel == "motor", 
                  is.na(getlunch))$rightfoot.iqr
))

# COUNT CHECK
all(c(
  length(dplyr::filter(dat, 
                  cellsource == "job", 
                  travel == "walk", 
                  getlunch == "home")$rightfoot) ==  
    dplyr::filter(aggregated.3CATS, 
                  cellsource == "job", 
                  travel == "walk", 
                  getlunch == "home")$count,
  
  length(dplyr::filter(dat, 
                  cellsource == "parent", 
                  travel == "other", 
                  getlunch == "tuckshop")$rightfoot) ==  
    dplyr::filter(aggregated.3CATS, 
                  cellsource == "parent", 
                  travel == "other", 
                  getlunch == "tuckshop")$count,
  
  length(dplyr::filter(dat, 
                  is.na(cellsource), 
                  travel == "motor", 
                  is.na(getlunch))$rightfoot) ==  
    dplyr::filter(aggregated.3CATS, 
                  is.na(cellsource), 
                  travel == "motor", 
                  is.na(getlunch))$count
))

###---------------------------------------------------------


# DATA OPTIONS -> STACK VARIABLES
###---------------------------------------------------------

stackVars = function(dat, vars){
  
}

# 1 VAR
stacked.1VARS <- dat %>% 
  tidyr::gather(key = stack.variable, value = stack.value, cellsource)
# stacked.1VARS = stackVars(dat, var)
head(stacked.1VARS)


# 2 VAR
stacked.2VARS <- dat %>% 
  tidyr::gather(key = stack.variable, value = stack.value, cellsource, travel)
# stacked.2VARS = stackVars(dat, var)
head(stacked.2VARS)


# 3 VAR
stacked.3VARS <- dat %>% 
  tidyr::gather(key = stack.variable, value = stack.value, cellsource, travel, getlunch)
# stacked.3VARS = stackVars(dat, var)
head(stacked.3VARS)


# 4 VAR
stacked.4VARS <- dat %>% 
  tidyr::gather(key = stack.variable, value = stack.value, cellsource, travel, getlunch, gender)
# stacked.4VARS = stackVars(dat, var)
head(stacked.4VARS)


# TEST CORRECT NUMBER OF ROWS
all(c(nrow(stacked.1VARS) == 1 * nrow(dat), 
      nrow(stacked.2VARS) == 2 * nrow(dat), 
      nrow(stacked.3VARS) == 3 * nrow(dat), 
      nrow(stacked.4VARS) == 4 * nrow(dat)))

# TEST CORRECT NUMBER OF ROWS PER STACKED VARIABLE
all(c(table(stacked.1VARS$stack.variable) == nrow(dat),
      table(stacked.2VARS$stack.variable) == nrow(dat),
      table(stacked.3VARS$stack.variable) == nrow(dat),
      table(stacked.4VARS$stack.variable) == nrow(dat)))

###---------------------------------------------------------


## I've also started some tests, which can be run using
test()

