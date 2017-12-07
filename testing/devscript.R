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


# ==========================================================================
# NOTES
# ==========================================================================
# - learn about "expressions"; i.e., ~x + 2
# - create an FORMULA for each of the examples - in most cases,
#   this will simply involve adding a ~ before it;
# - then run "interpolate()" on the expression, and see what happens.
exp <- ~(1:10 + 5) %>% mean()
x <- interpolate(exp)
x

y <- 1:10
exp <- ~mean(y)
x <- interpolate(exp)
x

# - next, how to pass variables?
exp <- ~mean(y)
x <- interpolate(exp, y = 1:10)
x

# - how about NAMES variables?
y <- 1:10
exp <- ~mean(.values)
x <- interpolate(exp, .values = y)
x

exp <- ~nchar(.var)
cat(code(interpolate(exp, .var = "string")), "\n")





### DATASET > FILTER LEVELS OF A CATEGORICAL
# filter travel = walk, bike

var <- "travel"
levels <- c("bike", "walk")

dat.filtered <- dat %>% 
  dplyr::filter(travel %in% levels) %>% 
  dplyr::mutate(travel = factor(travel, levels = levels))

filterLevels <- function(.data, var, levels) {
  ## allows us to get the NAME as passed into the function call
  ## e.g., filterLevels(mydata, ...), we get the STRING "mydata"
  
  mc <- match.call()
  dataname <- mc$.data

  exp <- 
    ~.DATA %>% 
      dplyr::filter(.VARNAME %in% .levels) %>% 
      dplyr::mutate(.VARNAME = factor(.VARNAME, levels = .levels))
  
  exp <- replaceVars(exp, .VARNAME = var, .DATA = dataname)

  #str <- as.character(exp)
  #str <- gsub(".VARNAME", var, str, fixed = TRUE)
  #str <- gsub(".DATA", dataname, str, fixed = TRUE)
  #exp <- as.formula(str)

  ## do the above using a function ...
  # exp <- replaceVars(exp, .VARNAME = var, .DATA = dataname)

  interpolate(exp, .levels = levels)
}

## example 
somedata <- dat
dat.filtered <- filterLevels(dat, "travel", c("bike", "walk"))
d2 <- filterLevels(somedata, "travel", c("bike", "walk"))
head(dat.filtered)
all(levels(dat.filtered$travel) %in% c("bike", "walk"))

formatR::tidy_source(text = code(dat.filtered), width.cutoff = 50) 
formatR::tidy_source(text = code(d2), width.cutoff = 50) 


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
    
# function for "<"
filterNumeric <- function(.data, var, op, num){
  mc <- match.call()
  dataname <- mc$.data
  
  ## NOTE: in this case, I'd rather see the values in the 
  #  function call; e.g., filter(.VARNAME .OP .value)
  #  then replaceVars for .VARNAME and .OP,
  #  then pass .value via interpolate.
  
  # updated to start with a string instead of a formula
  exp <- "~.DATA %>% dplyr::filter(.VARNAME.OP.NUM)"
  exp <- replaceVars(exp, 
                     .VARNAME = var, 
                     .OP = op,
                     .NUM = num,
                     .DATA = dataname)
  interpolate(exp)
}

# TEST FOR "<"
#filtered.L <- dat %>% 
#  dplyr::filter(rightfoot < 15)
filtered.L <- filterNumeric(dat, "rightfoot", "<", 15)
formatR::tidy_source(text = code(filtered.L), width.cutoff = 50) 
all(filtered.L$rightfoot < 15)
head(filtered.L)


# TEST FOR "<="
#filtered.LE <- dat %>% 
#  dplyr::filter(height <= 120)
filtered.LE <- filterNumeric(dat, "height", "<=", 120)
formatR::tidy_source(text = code(filtered.LE), width.cutoff = 50) 
all(filtered.LE$height <= 120)
head(filtered.LE)


# TEST FOR ">"
#filtered.G <- dat %>% 
#  dplyr::filter(age > 17)
filtered.G <- filterNumeric(dat, "age", ">", 17)
formatR::tidy_source(text = code(filtered.G), width.cutoff = 50) 
all(filtered.G$age > 17)
head(filtered.G)


# TEST FOR ">="
#filtered.GE <- dat %>% 
#  dplyr::filter(year >= 10)
filtered.GE <- filterNumeric(dat, "year", ">=", 10)
formatR::tidy_source(text = code(filtered.GE), width.cutoff = 50) 
all(filtered.GE$year >= 10)
head(filtered.GE)


# TEST FOR "=="
#filtered.E <- dat %>% 
#  dplyr::filter(armspan == 140)
filtered.E <- filterNumeric(dat, "armspan", "==", 140)
formatR::tidy_source(text = code(filtered.E), width.cutoff = 50) 
all(filtered.E$armspan == 140)
head(filtered.E)


# TEST FOR "!="
#filtered.NE <- dat %>% 
#  dplyr::filter(cellcost != 0)
filtered.NE <- filterNumeric(dat, "cellcost", "!=", 0)
formatR::tidy_source(text = code(filtered.NE), width.cutoff = 50) 
all(filtered.NE$cellcost != 0)
head(filtered.NE)

###---------------------------------------------------------


# DATASET -> FILTER DATASET -> ROW NUMBER
###---------------------------------------------------------

filterRow <- function(.data, rows){
  mc <- match.call()
  dataname <- mc$.data
  
  exp <- ~ .DATA %>% 
    dplyr::mutate(row.num = 1:nrow(.DATA)) %>% # this line is to show the row slicing was correct
      dplyr::slice(-.rows)
  exp <- replaceVars(exp, .DATA = dataname)
  interpolate(exp, .rows = rows)
}

#filtered.ROW <- dat %>%
#  dplyr::mutate(row.num = 1:nrow(dat)) %>%
#    dplyr::slice(-c(1,2,3,4,6,7,8,9))
  
filtered.ROW <- filterRow(dat, c(1,2,3,4,6,7,8,9))
formatR::tidy_source(text = code(filtered.ROW), width.cutoff = 50) 

# TEST CORRECT NUMBER OF ROWS
nrow(filtered.ROW) == nrow(dat) - length(c(1,2,3,4,6,7,8,9))


# TEST THAT ONLY ROW NUMBERS NOT REMOVED ARE KEPT
all(!(filtered.ROW$row.num %in% c(1,2,3,4,6,7,8,9)))
head(filtered.ROW)

###---------------------------------------------------------


# DATASET -> FILTER DATASET -> RANDOMLY
###---------------------------------------------------------

filterRandom <- function(.data, sample_size, n){
  mc <- match.call()
  dataname <- mc$.data
  
  exp <- ~ .DATA %>% 
    dplyr::mutate(row.num = 1:nrow(.DATA)) %>% # this line is to show that that the sampling was correct
      dplyr::sample_n(.sample_size * .n, replace = FALSE) %>%
        dplyr::mutate(Sample.Number = rep(1:.n, each=.sample_size))
  
  exp <- replaceVars(exp, .DATA = dataname)
  
  interpolate(exp, .sample_size = sample_size, .n = n)
}

sample_size = 5
n = 4
#filtered.RANDOM <- dat %>%
#  dplyr::mutate(row.num = 1:nrow(dat)) %>% 
#    dplyr::sample_n(sample_size * n, replace = FALSE) %>%
#      dplyr::mutate(Sample.Number = rep(1:n, each=sample_size))
filtered.RANDOM <- filterRandom(dat, sample_size, n)
formatR::tidy_source(text = code(filtered.RANDOM), width.cutoff = 50) 

# CHECK CORRECT NUMBER OF SAMPLES TAKEN
nrow(filtered.RANDOM) == sample_size * n

# CHECK ALL SAMPLES TAKEN ARE UNIQUE (ie.without replacement)
all(table(filtered.RANDOM$row.num) == 1)
head(filtered.RANDOM)


###---------------------------------------------------------


# DATASET -> SORT DATA BY VARIABLES
###---------------------------------------------------------

sortVars = function(.data, vars, asc = rep(TRUE, length(vars))) {
  mc <- match.call()
  dataname <- mc$.data
  
  # paste together variables names adding desc() where needed
  eval_str <- ifelse(asc, vars, stringr::str_c("desc(", vars, ")", sep = "")) %>%
    stringr::str_c(collapse = ", ")
  
  exp <- "~.DATA %>% 
    dplyr::arrange(.EVAL)"
  exp <- replaceVars(exp, .DATA = dataname, .EVAL = eval_str)
  
  interpolate(exp)
}

# SORT 1 VAR
#sorted.1VARS <- dat %>% 
# dplyr::arrange(cellsource)

sorted.1VARS <- sortVars(dat, c("cellsource"))
formatR::tidy_source(text = code(sorted.1VARS), width.cutoff = 50) 
head(sorted.1VARS, n= 15)

# SORT 2 VAR
#sorted.2VARS <- dat %>% 
#  dplyr::arrange(desc(rightfoot), travel)

sorted.2VARS <- sortVars(dat, c("rightfoot", "travel"), asc = c(FALSE, TRUE))
formatR::tidy_source(text = code(sorted.2VARS), width.cutoff = 50) 
head(sorted.2VARS, n = 15)

# SORT 3 VAR
#sorted.3VARS <- dat %>% 
#  dplyr::arrange(getlunch, desc(height), age)

sorted.3VARS <- sortVars(dat, c("getlunch", "height", "age"), asc = c(TRUE, FALSE, TRUE))
formatR::tidy_source(text = code(sorted.3VARS), width.cutoff = 50) 
head(sorted.3VARS, n = 15)

# SORT 4 VAR
#sorted.4VARS <- dat %>% 
#  dplyr::arrange(desc(year), armspan, desc(cellcost), gender)

sorted.4VARS <- sortVars(dat, c("year", "armspan", "cellcost", "gender"), asc = c(FALSE, TRUE, FALSE, TRUE))
formatR::tidy_source(text = code(sorted.4VARS), width.cutoff = 50) 
head(sorted.4VARS, n = 15)

###---------------------------------------------------------


# DATASET -> AGGREGATE DATA
###---------------------------------------------------------

# not sure how to check this is correct

# FUnction doesn't work for count yet
aggregateData = function(.data, .vars, .summaries){
  mc <- match.call()
  dataname <- mc$.data
  
  add_count = FALSE
  if("count" %in% .summaries){
    .summaries <- .summaries[.summaries != "count"]
    add_count = TRUE
  }
  
  sumamries = sort(.summaries)
  summaries_functionCall = ifelse(.summaries == "iqr", "IQR",.summaries)
  
  numeric_vars <- colnames(dplyr::select_if(.data, is.numeric)) %>%
    sort()
  
  # paste together the categorical variables for the group_by() statement
  groupby_str <- stringr::str_c(.vars, collapse = ", ")
  # paste together all the numeric variables and what summaries are requested for the summarize
  summarize_str <- stringr::str_c(rep(sort(numeric_vars), 
                                    each = length(.summaries)), 
                                  ".", 
                                  rep(.summaries, 
                                      length(numeric_vars)), 
                                  " = ",
                                  rep(summaries_functionCall, 
                                      length(numeric_vars)), 
                                  "(", rep(sort(numeric_vars), 
                                           each = length(.summaries)), 
                                  ", na.rm = TRUE)", collapse = ", ")
  
  if(add_count == TRUE){
    summarize_str <- stringr::str_c(summarize_str, "count = n()", sep = ", ")
  }
  
  exp <- ~.data %>%
    dplyr::group_by(.EVAL_GROUPBY) %>%
      dplyr::summarize(.EVAL_SUMMARIZE)
  
  exp <- replaceVars(exp, .EVAL_GROUPBY = groupby_str, .EVAL_SUMMARIZE = summarize_str)
  
  output <- interpolate(exp)
  
  return(output)
  
}

aggregated.3CATS = aggregateData(dat, c("cellsource", "travel", "getlunch"), c("sd", "mean", "median", "sum", "IQR"))
formatR::tidy_source(text = code(aggregated.3CATS), width.cutoff = 50) 

# aggregated.3CATS <- dat %>% 
#   dplyr::group_by(cellsource, travel, getlunch) %>%
#     dplyr::summarize(rightfoot.mean = mean(rightfoot, na.rm = TRUE),
#                      rightfoot.median = median(rightfoot, na.rm = TRUE),
#                      rightfoot.sum = sum(rightfoot, na.rm = TRUE),
#                      rightfoot.sd = sd(rightfoot, na.rm = TRUE),
#                      rightfoot.iqr = IQR(rightfoot, na.rm = TRUE),
#                      
#                      height.mean = mean(height, na.rm = TRUE),
#                      height.median = median(height, na.rm = TRUE),
#                      height.sum = sum(height, na.rm = TRUE),
#                      height.sd = sd(height, na.rm = TRUE),
#                      height.iqr = IQR(height, na.rm = TRUE),
#                      
#                      age.mean = mean(age, na.rm = TRUE),
#                      age.median = median(age, na.rm = TRUE),
#                      age.sum = sum(age, na.rm = TRUE),
#                      age.sd = sd(age, na.rm = TRUE),
#                      age.iqr = IQR(age, na.rm = TRUE),
#                      
#                      year.mean = mean(year, na.rm = TRUE),
#                      year.median = median(year, na.rm = TRUE),
#                      year.sum = sum(year, na.rm = TRUE),
#                      year.sd = sd(year, na.rm = TRUE),
#                      year.iqr = IQR(year, na.rm = TRUE),
#                      
#                      armspan.mean = mean(armspan, na.rm = TRUE),
#                      armspan.median = median(armspan, na.rm = TRUE),
#                      armspan.sum = sum(armspan, na.rm = TRUE),
#                      armspan.sd = sd(armspan, na.rm = TRUE),
#                      armspan.iqr = IQR(armspan, na.rm = TRUE),
#                      
#                      cellcost.mean = mean(cellcost, na.rm = TRUE),
#                      cellcost.median = median(cellcost, na.rm = TRUE),
#                      cellcost.sum = sum(cellcost, na.rm = TRUE),
#                      cellcost.sd = sd(cellcost, na.rm = TRUE),
#                      cellcost.iqr = IQR(cellcost, na.rm = TRUE),
#                      
#                      # Only one column for count makes sense
#                      count = n()
#                     )
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

stackVars = function(.data, .vars, 
    .key = "stack.variable", .value = "stack.value"){
  
  mc <- match.call()
  dataname <- mc$.data
  
  # paste together the variables to be stacked into a string
  to_be_stacked = stringr::str_c(.vars, collapse = ", ")
  
  exp <- ~.DATA %>% 
    tidyr::gather(key = .KEY, value = .VALUE, .VARNAMES)
  exp <- replaceVars(exp, .VARNAMES = to_be_stacked, .DATA = dataname)
  
  interpolate(exp, .KEY = .key, .VALUE = .value)  
}

# 1 VAR
#stacked.1VARS <- dat %>% 
#  tidyr::gather(key = stack.variable, value = stack.value, cellsource)
stacked.1VARS = stackVars(dat, c("cellsource"))
formatR::tidy_source(text = code(stacked.1VARS), width.cutoff = 50) 
head(stacked.1VARS)


# 2 VAR
#stacked.2VARS <- dat %>% 
#  tidyr::gather(key = stack.variable, value = stack.value, cellsource, travel)
stacked.2VARS = stackVars(dat, c("cellsource", "travel"))
head(stacked.2VARS)


# 3 VAR
#stacked.3VARS <- dat %>% 
#  tidyr::gather(key = stack.variable, value = stack.value, cellsource, travel, getlunch)
stacked.3VARS = stackVars(dat, c("cellsource", "travel", "getlunch"))
head(stacked.3VARS)


# 4 VAR
#stacked.4VARS <- dat %>% 
#  tidyr::gather(key = stack.variable, value = stack.value, cellsource, travel, getlunch, gender)
stacked.4VARS = stackVars(dat, c("cellsource", "travel", "getlunch", "gender"))
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


###---------------------------------------------------------
# VARIABLES -> CONVERT TO CATEGORICAL (allow a vector)

# WIP...
convertToCat <- function(.data, .vars){
  mc <- match.call()
  dataname <- mc$.data
  
  exp_str = c()
  
  for (i in 1:length(.vars)){
    exp_str[i] <- stringr::str_c("tibble::add_column(", .vars[i], ".CAT = factor(.DATA$", .vars[i], ", .after = ", .vars[i], ")",sep = "")
  }
  exp_str <- stringr::str_c(exp_str, collapse = " %>% \n ")
  exp_str <- stringr::str_c("tibble::add_column(".vars, ".CAT = factor(.DATA$", .vars, ")", collapse = ", ")
  
  exp <- ~.DATA %>%
    tibble::add_column(EXAM.cat = factor(stats20x.df$EXAM), .after = "EXAM")   
}

###---------------------------------------------------------


###---------------------------------------------------------
# VARIABLES -> CATEGORICAL VARIABLES -> REORDER LEVELS

reorderLevels <- function(.data, var, new_levels, freq = FALSE){
  mc <- match.call()
  dataname <- mc$.data
  
  if (freq){
    exp <- .DATA %>%
      tibble::add_column(.VARNAME.reord = forcats::fct_infreq(.DATA$.VARNAME), .after = ".VARNAME") 
  }
  else{
    exp <- ~.DATA %>%
      tibble::add_column(.VARNAME.reord = factor(.DATA$.VARNAME, levels = new_levels), .after = ".VARNAME") 
  }
  exp <- replaceVars(exp, .VARNAME = var)
}

## I've also started some tests, which can be run using
test()

