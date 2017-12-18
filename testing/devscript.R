## The idea of this script is to use it as a 'playground' while writing R package functions
#
# install.packages('devtools')  ## -- install devtools package if you haven't already

## Ideally, you won't import any other libraries this way ...
library(devtools)
library(magrittr)
library(stringr)
library(testthat)

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

filterLevels <- function(.data, var, levels) {
  
  mc <- match.call()
  dataname <- mc$.data

  exp <- 
    ~.DATA %>% 
      dplyr::filter(.VARNAME %in% .levels) %>% 
      dplyr::mutate(.VARNAME = factor(.VARNAME, levels = .levels))
  
  exp <- replaceVars(exp, .VARNAME = var, .DATA = dataname)

  interpolate(exp, .levels = levels)
}

# 1 LEVEL
filtered.1LVL <- dat %>% 
  dplyr::filter(cellsource %in% c("job")) %>% 
    dplyr::mutate(cellsource = factor(cellsource, levels = c("job")))
formatR::tidy_source(text = code(filtered.1LVL), width.cutoff = 50) 

# 3 LEVELS
filtered.3LVL <- dat %>% 
  dplyr::filter(getlunch %in% c("home", "tuckshop")) %>% 
    dplyr::mutate(getlunch = factor(getlunch, levels = c("home", "tuckshop", "friend")))
formatR::tidy_source(text = code(filtered.3LVL), width.cutoff = 50) 

# TEST_THAT'S
test_that("Result is only those with the filtered variable remains", {
  expect_true(all(levels(filtered.1LVL$cellsource) == c("job")))
  expect_true(all(levels(filtered.3LVL$getlunch) == c("home", "tuckshop", "friend")))
  
})

test_that("Result is the other categorical variables remain", {
  expect_true(levels(filtered.1LVL$travel) == levels(dat$travel)) 
  expect_true(levels(filtered.1LVL$getlunch) == levels(dat$getlunch))
  expect_true(levels(filtered.1LVL$gender) == levels(dat$gender))
  expect_true(levels(filtered.3LVL$cellsource) == levels(dat$cellsource)) 
  expect_true(levels(filtered.3LVL$travel) == levels(dat$travel))
  expect_true(levels(filtered.3LVL$gender) == levels(dat$gender))
  
})
###---------------------------------------------------------


# DATASET -> FILTER DATASET -> NUMERIC CONDITION
###---------------------------------------------------------
    
filterNumeric <- function(.data, var, op, num){
  mc <- match.call()
  dataname <- mc$.data
  
  exp <- ~.DATA %>% dplyr::filter(.VARNAME.OP.NUM)
  exp <- replaceVars(exp, 
                     .VARNAME = var, 
                     .OP = op,
                     .NUM = num,
                     .DATA = dataname)
  interpolate(exp)
}

# FOR "<"
filtered.L <- filterNumeric(dat, "rightfoot", "<", 15)
formatR::tidy_source(text = code(filtered.L), width.cutoff = 50) 

# FOR "<="
filtered.LE <- filterNumeric(dat, "height", "<=", 120)
formatR::tidy_source(text = code(filtered.LE), width.cutoff = 50) 

# FOR ">"
filtered.G <- filterNumeric(dat, "age", ">", 17)
formatR::tidy_source(text = code(filtered.G), width.cutoff = 50) 

# FOR ">="
filtered.GE <- filterNumeric(dat, "year", ">=", 10)
formatR::tidy_source(text = code(filtered.GE), width.cutoff = 50) 

# FOR "=="
filtered.E <- filterNumeric(dat, "armspan", "==", 140)
formatR::tidy_source(text = code(filtered.E), width.cutoff = 50) 

# FOR "!="
filtered.NE <- filterNumeric(dat, "cellcost", "!=", 0)
formatR::tidy_source(text = code(filtered.NE), width.cutoff = 50) 


# TEST_THAT'S
test_that("Result is that only those that meet the logical condition remain", {
  expect_true(all(filtered.L$rightfoot < 15))
  expect_true(all(filtered.LE$height <= 120))
  expect_true(all(filtered.G$age > 17))
  expect_true(all(filtered.GE$year >= 10))
  expect_true(all(filtered.E$armspan == 140))
  expect_true(all(filtered.NE$cellcost != 0))
})


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
  
filtered.ROW <- filterRow(dat, c(1,2,3,4,6,7,8,9))
formatR::tidy_source(text = code(filtered.ROW), width.cutoff = 50) 


# TEST_THAT'S
test_that("Result is the number of rows are reduced by the rows sliced", {
  expect_equal(nrow(filtered.ROW), nrow(dat) - length(c(1,2,3,4,6,7,8,9)))
})

test_that("Result is that the rows not sliced are kept", {
  expect_true(all(!(filtered.ROW$row.num %in% c(1,2,3,4,6,7,8,9))))
})

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
filtered.RANDOM <- filterRandom(dat, sample_size, n)
formatR::tidy_source(text = code(filtered.RANDOM), width.cutoff = 50) 

# TEST_THAT'S
test_that("Result is that all samples taken are unique", {
  expect_equal(nrow(filtered.RANDOM), sample_size * n)
})

test_that("Result is that the correct number of samples are taken for each group", {
  expect_true(all(table(filtered.RANDOM$row.num) == 1))
})
###---------------------------------------------------------


# DATASET -> SORT DATA BY VARIABLES
###---------------------------------------------------------

sortVars = function(.data, vars, asc = rep(TRUE, length(vars))) {
  mc <- match.call()
  dataname <- mc$.data
  
  # paste together variables names adding desc() where needed
  eval_str <- ifelse(asc, vars, str_c("desc(", vars, ")", sep = "")) %>%
    str_c(collapse = ", ")
  
  exp <- ~.DATA %>% 
    dplyr::arrange(.EVAL)
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

aggregateData = function(.data, .vars, .summaries){
  
  # function for counting the missing values
  countMissing <- function(var, na.rm = FALSE){
    sum(is.na(var))
  }
  
  mc <- match.call()
  dataname <- mc$.data
  
  summary_names <- .summaries %>% 
    sort() %>%
      c("missing")

  summaries_functionCall <- ifelse(.summaries == "iqr", "IQR", .summaries) %>%
    sort() %>%
      c("countMissing")
  
  numeric_vars <- colnames(dplyr::select_if(.data, is.numeric)) %>%
    sort()
  
  # paste together the categorical variables for the group_by() statement
  groupby_str <- str_c(.vars, collapse = ", ")
  # paste together all the numeric variables and what summaries are requested for the summarize
  summarize_str <- str_c(rep(sort(numeric_vars), 
                                  each = length(summary_names)), 
                                  ".", 
                                  rep(summary_names, 
                                      length(numeric_vars)), 
                                  " = ",
                                  rep(summaries_functionCall, 
                                      length(numeric_vars)), 
                                  "(", rep(sort(numeric_vars), 
                                           each = length(summary_names)), 
                                  ", na.rm = TRUE)", collapse = ", ")
  
  summarize_str <- str_c("count = n(), ", summarize_str)
  
  exp <- ~.data %>%
    dplyr::group_by(.EVAL_GROUPBY) %>%
      dplyr::summarize(.EVAL_SUMMARIZE)
  
  exp <- replaceVars(exp, .EVAL_GROUPBY = groupby_str, .EVAL_SUMMARIZE = summarize_str)
  
  output <- interpolate(exp)
  
  return(output)
}

aggregated.3CATS = aggregateData(dat, c("cellsource", "travel", "getlunch"), c("sd", "mean", "median", "sum", "iqr"))
formatR::tidy_source(text = code(aggregated.3CATS), width.cutoff = 50) 


# TEST_THAT'S
test_that("Spot check for the mean values", {
  expect_true(mean(dplyr::filter(dat, 
                                 cellsource == "job", 
                                 travel == "walk", 
                                 getlunch == "none")$rightfoot, 
                   na.rm = TRUE) ==  
                dplyr::filter(aggregated.3CATS, 
                              cellsource == "job", 
                              travel == "walk", 
                              getlunch == "none")$rightfoot.mean)
  expect_true(mean(dplyr::filter(dat, 
                                 cellsource == "parent", 
                                 travel == "other", 
                                 getlunch == "tuckshop")$cellcost, 
                   na.rm = TRUE) ==  
                dplyr::filter(aggregated.3CATS, 
                              cellsource == "parent", 
                              travel == "other", 
                              getlunch == "tuckshop")$cellcost.mean)
  expect_true(mean(dplyr::filter(dat, 
                                 is.na(cellsource), 
                                 travel == "motor", 
                                 getlunch == "home")$age, 
                   na.rm = TRUE) ==  
                dplyr::filter(aggregated.3CATS, 
                              is.na(cellsource), 
                              travel == "motor", 
                              getlunch == "home")$age.mean)
})

test_that("Spot check for the median values", {
  expect_true(median(dplyr::filter(dat, 
                                   cellsource == "job", 
                                   travel == "walk", 
                                   getlunch == "none")$rightfoot, 
                     na.rm = TRUE) ==  
                dplyr::filter(aggregated.3CATS, 
                              cellsource == "job", 
                              travel == "walk", 
                              getlunch == "none")$rightfoot.median)
  expect_true(median(dplyr::filter(dat, 
                                   cellsource == "parent", 
                                   travel == "other", 
                                   getlunch == "tuckshop")$cellcost, 
                     na.rm = TRUE) ==  
                dplyr::filter(aggregated.3CATS, 
                              cellsource == "parent", 
                              travel == "other", 
                              getlunch == "tuckshop")$cellcost.median)
  expect_true(median(dplyr::filter(dat, 
                                   is.na(cellsource), 
                                   travel == "motor", 
                                   getlunch == "home")$age, 
                     na.rm = TRUE) ==  
                dplyr::filter(aggregated.3CATS, 
                              is.na(cellsource), 
                              travel == "motor", 
                              getlunch == "home")$age.median)
})

test_that("Spot check for the sum values", {
  expect_true(sum(dplyr::filter(dat, 
                                cellsource == "job", 
                                travel == "walk", 
                                getlunch == "none")$rightfoot, 
                  na.rm = TRUE) ==  
                dplyr::filter(aggregated.3CATS, 
                              cellsource == "job", 
                              travel == "walk", 
                              getlunch == "none")$rightfoot.sum)
  expect_true(sum(dplyr::filter(dat, 
                                cellsource == "parent", 
                                travel == "other", 
                                getlunch == "tuckshop")$cellcost, 
                  na.rm = TRUE) ==  
                dplyr::filter(aggregated.3CATS, 
                              cellsource == "parent", 
                              travel == "other", 
                              getlunch == "tuckshop")$cellcost.sum)
  expect_true(sum(dplyr::filter(dat, 
                                is.na(cellsource), 
                                travel == "motor", 
                                getlunch == "home")$age, 
                  na.rm = TRUE) ==  
                dplyr::filter(aggregated.3CATS, 
                              is.na(cellsource), 
                              travel == "motor", 
                              getlunch == "home")$age.sum)
})


test_that("Spot check for the sd values", {
  expect_true(sd(dplyr::filter(dat, 
                               cellsource == "job", 
                               travel == "walk", 
                               getlunch == "none")$rightfoot, 
                 na.rm = TRUE) ==  
                dplyr::filter(aggregated.3CATS, 
                              cellsource == "job", 
                              travel == "walk", 
                              getlunch == "none")$rightfoot.sd)
  expect_true(sd(dplyr::filter(dat, 
                                cellsource == "parent", 
                                travel == "other", 
                                getlunch == "tuckshop")$cellcost, 
                  na.rm = TRUE) ==  
                 dplyr::filter(aggregated.3CATS, 
                               cellsource == "parent", 
                               travel == "other", 
                               getlunch == "tuckshop")$cellcost.sd)
  expect_true(sd(dplyr::filter(dat, 
                               is.na(cellsource), 
                               travel == "motor", 
                               getlunch == "home")$age, 
                 na.rm = TRUE) ==  
                dplyr::filter(aggregated.3CATS, 
                              is.na(cellsource), 
                              travel == "motor", 
                              getlunch == "home")$age.sd)
})


test_that("Spot check for the IQR values", {
  expect_true(IQR(dplyr::filter(dat, 
                                cellsource == "job", 
                                travel == "walk", 
                                getlunch == "none")$rightfoot, 
                  na.rm = TRUE) ==  
                dplyr::filter(aggregated.3CATS, 
                              cellsource == "job", 
                              travel == "walk", 
                              getlunch == "none")$rightfoot.iqr)
  expect_true(IQR(dplyr::filter(dat, 
                                cellsource == "parent", 
                                travel == "other", 
                                getlunch == "tuckshop")$cellcost, 
                  na.rm = TRUE) ==  
                dplyr::filter(aggregated.3CATS, 
                              cellsource == "parent", 
                              travel == "other", 
                              getlunch == "tuckshop")$cellcost.iqr)
  expect_true(IQR(dplyr::filter(dat, 
                                is.na(cellsource), 
                                travel == "motor", 
                                getlunch == "home")$age, 
                  na.rm = TRUE) ==  
                dplyr::filter(aggregated.3CATS, 
                              is.na(cellsource), 
                              travel == "motor", 
                              getlunch == "home")$age.iqr)
})


test_that("Spot check for the total count values", {
  expect_true(length(dplyr::filter(dat, 
                                   cellsource == "job", 
                                   travel == "walk", 
                                   getlunch == "none")$rightfoot) ==  
                dplyr::filter(aggregated.3CATS, 
                              cellsource == "job", 
                              travel == "walk", 
                              getlunch == "none")$count)
  expect_true(length(dplyr::filter(dat, 
                                   cellsource == "parent", 
                                   travel == "other", 
                                   getlunch == "tuckshop")$cellcost) ==  
                dplyr::filter(aggregated.3CATS, 
                              cellsource == "parent", 
                              travel == "other", 
                              getlunch == "tuckshop")$count)
  expect_true(length(dplyr::filter(dat, 
                                   is.na(cellsource), 
                                   travel == "motor", 
                                   getlunch == "home")$age) ==  
                dplyr::filter(aggregated.3CATS, 
                              is.na(cellsource), 
                              travel == "motor", 
                              getlunch == "home")$count)
})

test_that("Spot check for the missing count values", {
  expect_true(sum(is.na(dplyr::filter(dat, 
                                      cellsource == "job", 
                                      travel == "walk", 
                                      getlunch == "none")$rightfoot)) ==
                    dplyr::filter(aggregated.3CATS, 
                                  cellsource == "job", 
                                  travel == "walk", 
                                  getlunch == "none")$rightfoot.missing)
  expect_true(sum(is.na(dplyr::filter(dat, 
                                      cellsource == "parent", 
                                      travel == "other", 
                                      getlunch == "tuckshop")$cellcost)) ==
                dplyr::filter(aggregated.3CATS, 
                              cellsource == "parent", 
                              travel == "other", 
                              getlunch == "tuckshop")$cellcost.missing)
  expect_true(sum(is.na(dplyr::filter(dat, 
                                      is.na(cellsource), 
                                      travel == "motor", 
                                      getlunch == "home")$age)) ==
                dplyr::filter(aggregated.3CATS, 
                              is.na(cellsource), 
                              travel == "motor", 
                              getlunch == "home")$age.missing)
})
###---------------------------------------------------------


# DATA OPTIONS -> STACK VARIABLES
###---------------------------------------------------------

stackVars = function(.data, .vars, 
    .key = "stack.variable", .value = "stack.value"){
  
  mc <- match.call()
  dataname <- mc$.data
  
  # paste together the variables to be stacked into a string
  to_be_stacked = str_c(.vars, collapse = ", ")
  
  exp <- ~.DATA %>% 
    tidyr::gather(key = .KEY, value = .VALUE, .VARNAMES)
  exp <- replaceVars(exp, .VARNAMES = to_be_stacked, .DATA = dataname)
  
  interpolate(exp, .KEY = .key, .VALUE = .value)  
}

# 1 VAR
stacked.1VARS = stackVars(dat, c("cellsource"))
formatR::tidy_source(text = code(stacked.1VARS), width.cutoff = 50) 

# 2 VAR
stacked.2VARS = stackVars(dat, c("cellsource", "travel"))
formatR::tidy_source(text = code(stacked.2VARS), width.cutoff = 50) 

# 3 VAR
stacked.3VARS = stackVars(dat, c("cellsource", "travel", "getlunch"))
formatR::tidy_source(text = code(stacked.3VARS), width.cutoff = 50) 

# 4 VAR
stacked.4VARS = stackVars(dat, c("cellsource", "travel", "getlunch", "gender"))
formatR::tidy_source(text = code(stacked.4VARS), width.cutoff = 50) 


# TEST CORRECT NUMBER OF ROWS PER STACKED VARIABLE
all(c(table(stacked.1VARS$stack.variable) == nrow(dat),
      table(stacked.2VARS$stack.variable) == nrow(dat),
      table(stacked.3VARS$stack.variable) == nrow(dat),
      table(stacked.4VARS$stack.variable) == nrow(dat)))


# TEST_THAT'S
test_that("Result is the number of rows is correct", {
  expect_equal(nrow(stacked.1VARS), 1 * nrow(dat))
  expect_equal(nrow(stacked.2VARS), 2 * nrow(dat))
  expect_equal(nrow(stacked.3VARS), 3 * nrow(dat))
  expect_equal(nrow(stacked.4VARS), 4 * nrow(dat))
})

test_that("Result is the number of rows per stacked variable is correct", {
  expect_true(all(table(stacked.1VARS$stack.variable) == nrow(dat)))
  expect_true(all(table(stacked.2VARS$stack.variable) == nrow(dat)))
  expect_true(all(table(stacked.3VARS$stack.variable) == nrow(dat)))
  expect_true(all(table(stacked.4VARS$stack.variable) == nrow(dat)))
})


###---------------------------------------------------------


###---------------------------------------------------------
# VARIABLES -> CONVERT TO CATEGORICAL (allow a vector)
convertToCat <- function(.data, vars){
  mc <- match.call()
  dataname <- mc$.data
  
  formulae = list(~.DATA)
  
  for (i in 1:length(vars)){
    formula <- ~tibble::add_column(.VARNAME.cat = factor(.DATA$.VARNAME), .after = ".VARNAME")
    formula <- replaceVars(formula, .VARNAME = vars[i])
    formulae[[i+1]] <- formula
  }
  
  exp <- pasteFormulae(formulae)
  exp <- replaceVars(exp, .DATA = dataname)
  
  interpolate(exp)
}

var.CAT <- convertToCat(dat, c("armspan", "height", "age"))


# TEST_THAT'S
test_that("Result is the new categorical variables are factors", {
  expect_type(levels(var.CAT$armspan.cat),"character")
  expect_type(levels(var.CAT$height.cat), "character")
  expect_type(levels(var.CAT$age.cat), "character")
})

test_that("Result is the factor levels of the new categorical variables are correct", {
  expect_true(all(var.CAT$armspan.cat %>%
                    levels() %>%
                      as.numeric() ==
                    var.CAT$armspan %>%
                      unique() %>%
                        sort()))
  expect_true(all(var.CAT$height.cat %>%
                    levels() %>%
                      as.numeric() ==
                    var.CAT$height %>%
                      unique() %>%
                        sort()))
  expect_true(all(var.CAT$age.cat %>%
                    levels() %>%
                      as.numeric() ==
                    var.CAT$age %>%
                      unique() %>%
                        sort()))
})

test_that("Result is the original numeric variables are not factors", {
  expect_null(levels(var.CAT$armspan))
  expect_null(levels(var.CAT$height))
  expect_null(levels(var.CAT$age))
})

###---------------------------------------------------------


###---------------------------------------------------------
# VARIABLES -> CATEGORICAL VARIABLES -> REORDER LEVELS

reorderLevels <- function(.data, var, new_levels = NULL, freq = FALSE){
  mc <- match.call()
  dataname <- mc$.data
  
  if(freq){
    exp <- ~.DATA %>%
      tibble::add_column(.VARNAME.reord = forcats::fct_infreq(.DATA$.VARNAME), .after = ".VARNAME") 
  }
  else{
    exp <- ~.DATA %>%
      tibble::add_column(.VARNAME.reord = factor(.DATA$.VARNAME, levels = new_levels), .after = ".VARNAME") 
  }
  exp <- replaceVars(exp, .VARNAME = var, .DATA = dataname)
  
  interpolate(exp)
}


cat.REORDER.MANUAL1 <- reorderLevels(dat, "cellsource", c("parent", "pocket", "job", "other"))
formatR::tidy_source(text = code(cat.REORDER.MANUAL1), width.cutoff = 50) 

cat.REORDER.MANUAL2 <- reorderLevels(dat, "travel", c("other", "train", "motor", "bus", "walk", "bike"))

cat.REORDER.FREQ1 <- reorderLevels(dat, "cellsource", freq = TRUE)
formatR::tidy_source(text = code(cat.REORDER.FREQ1), width.cutoff = 50)

cat.REORDER.FREQ2 <- reorderLevels(dat, "travel", freq = TRUE)


# TEST_THAT'S
test_that("Check that the reordered factor levels are correct", {
  expect_true(all(levels(cat.REORDER.MANUAL1$cellsource.reord) == 
                     c("parent", "pocket", "job", "other")))
  expect_true(all(levels(cat.REORDER.MANUAL2$travel.reord) ==
                    c("other", "train", "motor", "bus", "walk", "bike")))
  expect_true(all(diff(table(cat.REORDER.FREQ1$cellsource.reord)) <= 0))
  expect_true(all(diff(table(cat.REORDER.FREQ2$travel.reord)) <= 0))
})

###---------------------------------------------------------


###---------------------------------------------------------
# VARIABLES -> CATEGORICAL VARIABLES -> COLLAPSE LEVELS

collapseLevels <- function(.data, var, levels){
  mc <- match.call()
  dataname <- mc$.data
  
  exp <- ~.DATA %>%
    tibble::add_column(.VARNAME.coll = forcats::fct_collapse(.DATA$.VARNAME, .COLLAPSENAME = .LEVELS), .after = ".VARNAME")
  exp <- replaceVars(exp, .VARNAME = var, .COLLAPSENAME = str_c(levels, collapse = "_"), .LEVELS = levels, .DATA = dataname)
  
  interpolate(exp)
}

cat.COLLAPSE1 <- collapseLevels(dat, "getlunch", c("dairy", "school", "tuckshop"))
formatR::tidy_source(text = code(cat.COLLAPSE1), width.cutoff = 50) 

cat.COLLAPSE2 <- collapseLevels(dat, "gender", c("female", "male"))


# TEST_THAT'S
test_that("Result is that the factor levels in the new collpased variable contains the collapsed values", {
  expect_true("dairy_school_tuckshop" %in% levels(cat.COLLAPSE1$getlunch.coll))
  expect_true("female_male" %in% levels(cat.COLLAPSE2$gender.coll))
})

test_that("Result is that the old factors used to be combined are removed", {
  expect_false("dairy" %in% levels(cat.COLLAPSE1$getlunch.coll))
  expect_false("school" %in% levels(cat.COLLAPSE1$getlunch.coll))
  expect_false("tuckshop" %in% levels(cat.COLLAPSE1$getlunch.coll))
  expect_false("female" %in% levels(cat.COLLAPSE2$gender.coll))
  expect_false("male" %in% levels(cat.COLLAPSE2$gender.coll))
})

test_that("Result is that levels not mentioned remain unchanged"{
  expect_true("friend" %in% levels(cat.COLLAPSE1$getlunch.coll))
  expect_true("home" %in% levels(cat.COLLAPSE1$getlunch.coll))
  expect_true("none" %in% levels(cat.COLLAPSE1$getlunch.coll))
})
###---------------------------------------------------------


###---------------------------------------------------------
# VARIABLES -> CATEGORICAL VARIABLES -> RENAME LEVELS

renameLevels <- function(.data, var, to_be_renamed){
  mc <- match.call()
  dataname <- mc$.data
  
  # paste together the new and old factor levels names to be renamed
  to_be_renamed <- str_c(names(to_be_renamed), ' = "', to_be_renamed, '"', collapse = ", ")
  
  exp <- ~.DATA %>%
    tibble::add_column(.VARNAME.rename = forcats::fct_recode(.DATA$.VARNAME, .RENAME), .after = ".VARNAME")
  exp <- replaceVars(exp, .DATA = dataname ,.RENAME = to_be_renamed, .VARNAME = var)
  
  interpolate(exp)
}

cat.RENAME <- renameLevels(dat, "travel", list(public = "bus", private = "motor"))
formatR::tidy_source(text = code(cat.RENAME), width.cutoff = 50) 

###---------------------------------------------------------


###---------------------------------------------------------
# VARIABLES -> CATEOGIRCAL VARIABLES -> COMBINE CATEGORICAL VARIABLES

combineCatVars <- function(.data, vars, sep = "."){
  mc <- match.call()
  dataname <- mc$.data
  
  # paste together the new variable made from the old variable names 
  new_var_name <- str_c(vars, collapse = sep)
  to_be_combined <- str_c(vars, collapse =", ")
  
  exp <- ~.DATA %>%
      dplyr::mutate(.NEWVAR = str_c(.VARS, sep = ".SEP"))
  exp <- replaceVars(exp, .DATA = dataname, .NEWVAR = new_var_name, .VARS = to_be_combined, .SEP = sep)
  
  interpolate(exp)
}

cat.COMBINE <- combineCatVars(dat, c("travel", "getlunch", "gender"), ".0.")
formatR::tidy_source(text = code(cat.COMBINE), width.cutoff = 50) 

###---------------------------------------------------------


###---------------------------------------------------------
# VARIABLES -> NUMERIC VARIABLES -> TRANSFORM VARIABLES

# this function should work with all the variable transformations in iNZight provided the name given is correct

transformVar <- function(.data, var ,transformation){
  
  # Below are custom functions for transform variables
  
  reciprocal <- function(x){
    return(1/x)
  }
  square <- function(x){
    return (x ^ 2)
  }
  
  mc <- match.call()
  dataname <- mc$.data
  
  exp <- ~.DATA %>%
    tibble::add_column(.VARNAME..F = lapply(.DATA$.VARNAME, .F), .after = ".VARNAME")
  exp <- replaceVars(exp, .DATA = dataname, .VARNAME = var, .F = transformation)
  
  interpolate(exp)
}

num.TRANSFORM <- transformVar(dat, "armspan", "exp")
formatR::tidy_source(text = code(num.TRANSFORM), width.cutoff = 50) 
###---------------------------------------------------------


###---------------------------------------------------------
# VARIABLES -> NUMERIC VARIABLES -> STANDARDIZE VARIABLES

standardizeVars <- function(.data, vars){
  mc <- match.call()
  dataname <- mc$.data
  
  formulae <- list(~.DATA)
    
  for (i in 1:length(vars)){
    formula <- ~ tibble::add_column(.VARNAME.std = scale(.DATA$.VARNAME)[,1], .after = ".VARNAME")
    formulae[[i+1]] <- replaceVars(formula, .VARNAME = vars[i])
  }
  
  exp <- pasteFormulae(formulae)
  exp <- replaceVars(exp, .DATA = dataname)
  
  interpolate(exp)
}

num.STANDARDIZE <- standardizeVars(dat, c("cellcost", "height", "year"))
formatR::tidy_source(text = code(num.STANDARDIZE), width.cutoff = 50) 
###---------------------------------------------------------


###---------------------------------------------------------
# VARIABLES -> NUMERIC VARIABLES -> FORM CLASS INTERVALS

formClassIntervals <- function(.data, var, new_var, nlevels, new_level, method){
  mc <- match.call()
  dataname <- mc$.data
  
  min <- .data %>% dplyr::select(var) %>% min(na.rm = TRUE) %>% floor()
  max <- .data %>% dplyr::select(var) %>% max(na.rm = TRUE) %>% ceiling()
  
  width = (max - min) / nlevels
  
  intervals = seq(min, max, width)
  intervals_rounded = round(intervals, 2)
    
  if(new_level = "open left closed right"){
    interval_names = str_c("(", intervals_rounded[-length(intervals_rounded)], ", ", intervals_rounded[-1], "]")
    interval_allocation <- .data %>% 
        dplyr::select(var) %>%
          unlist() %>%
            as.numeric() %>%
              findInterval(intervals, left.open = TRUE)
    
  }
  else{
    interval_names = str_c("[", intervals_rounded[-length(intervals_rounded)], ", ", intervals_rounded[-1], ")")
    interval_allocation <- .data %>% 
      dplyr::select(var) %>%
        unlist() %>%
          as.numeric() %>%
            findInterval(intervals, rightmost.closed, left.open = FALSE)
  }
  
  vlookup.df = data.frame(group = 1:nlevels, interval = interval_names)
  allocated_intervals.df = merge(data.frame(group = interval_allocation), vlookup.df, by = "group")
  
  
}


###---------------------------------------------------------





###---------------------------------------------------------
# VARIABLES -> NUMERIC VARIABLES -> RANK NUMERICAL VARIABLES

rankVars <- function(.data, vars){
  mc <- match.call()
  dataname <- mc$.data
  
  formulae <- list(~.DATA)
  
  for (i in 1:length(vars)){
    formula <- ~ tibble::add_column(.VARNAME.rank = dplyr::min_rank(.DATA$.VARNAME), .after = ".VARNAME")
    formulae[[i+1]] <- replaceVars(formula, .VARNAME = vars[i])
  }
  
  exp <- pasteFormulae(formulae)
  exp <- replaceVars(exp, .DATA = dataname)
  
  interpolate(exp)
}

num.RANK <- rankVars(dat, c("armspan", "year"))
formatR::tidy_source(text = code(num.RANK), width.cutoff = 50) 
###---------------------------------------------------------




###---------------------------------------------------------
# VARIABLES -> NUMERIC VARIABLES -> CONVERT TO CATEGORICAL (MULTIPLE VARIABLES)

# see VARIABLES -> NUMERIC VARIABLES -> CONVERT TO CATEGORICAL

###---------------------------------------------------------


###---------------------------------------------------------
# VARIABLES -> RENAME VARIABLES 

renameVars <- function(.data, to_be_renamed_list){
  mc <- match.call()
  dataname <- mc$.data
  
  # paste together the variables to be renamed into one string
  to_be_renamed <- str_c(to_be_renamed_list, "=", names(to_be_renamed_list), collapse = ", ")
  
  exp <- ~.DATA %>%
    dplyr::rename(.RENAME)
  exp <- replaceVars(exp, .RENAME = to_be_renamed, .DATA = dataname)
  
  interpolate(exp)
}

var.RENAME <- renameVars(dat, list(gender = "SEX", getlunch = "LUNCH_SOURCE", travel = "TRANSPORTATION"))
formatR::tidy_source(text = code(var.RENAME), width.cutoff = 50) 
###---------------------------------------------------------


###---------------------------------------------------------
# VARIABLES -> CREATE NEW VARIABLES 

createNewVar <- function(.data, new_var = "new.variable", R_exp){
  mc <- match.call()
  dataname <- mc$.data
  
  exp <- ~.DATA %>%
    dplyr::mutate(.VARNAME = .EVAL)
  exp <- replaceVars(exp, .VARNAME = new_var, .EVAL = R_exp)#, DATA = dataname)
  
  interpolate(exp)
  
}

var.NEW = createNewVar(dat, new_var = "EVEN_OR_ODD_AGE", '(year +5) / 3')
formatR::tidy_source(text = code(var.NEW), width.cutoff = 50) 

###---------------------------------------------------------


###---------------------------------------------------------
# VARIABLES -> MISSING TO CATEGORICAL 

missingToCat <- function(.data, vars){
  mc <- match.call()
  dataname <- mc$.data
  
  formulae <- list(~.DATA)
  
  for (i in 1:length(vars)){
    if(is.numeric(dplyr::select(.data, vars[i])[,1])){
      formula <- ~tibble::add_column(.VARNAME_miss = factor(ifelse(is.na(.DATA$.VARNAME),"missing", "observed")), .after = ".VARNAME")  
    }
    else{
      formula <- ~ tibble::add_column(.VARNAME_miss = forcats::fct_explicit_na(.DATA$.VARNAME, na_level = "missing"), .after = ".VARNAME")
    }
    formula <- replaceVars(formula, .VARNAME = vars[i])
    formulae[[i+1]] = formula
  }
  
  exp <- pasteFormulae(formulae)
  exp <- replaceVars(exp, .DATA = dataname)
  
  interpolate(exp)
}

var.MISSING <- missingToCat(dat, c("getlunch", "age", "year", "travel"))

###---------------------------------------------------------


###---------------------------------------------------------
# VARIABLES -> RESHAPE DATASET

# already a function to do this - see DATASET -> STACK VARIABLES

###---------------------------------------------------------



## I've also started some tests, which can be run using
test()

