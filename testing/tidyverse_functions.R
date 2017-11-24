library(magrittr)
#library(dplyr)
#library(stringr)

setwd("D:/OneDrive/University of Auckland/Summer Research Scholarship/iNZight work")

stats20x.df <- read.csv("stats20x.csv", header = TRUE,  
                        na.strings = c("NULL","NA", "N/A","#N/A","","<NA>"))

# DATASET -> FILTER DATASET -> LEVELS OF A CATEGORICAL VARIABLE
###---------------------------------------------------------

# example : filtering for column "DEGREE" for only obs with "BCom", "BA" or "Other"
filtered.DEGREE <- stats20x.df %>% 
  dplyr::filter(DEGREE %in% c("BCom", "BA", "Other"))

# remove unwanted factors
filtered.DEGREE = filtered.DEGREE %>% dplyr::mutate(DEGREE = factor(DEGREE, levels = c("BCom", "BA", "Other")))
###

## things to check:
# 1. any OTHER factors keep empty levels - add some arbitrary level to a factor
levels(stats20x.df$GENDER) <- c(levels(stats20x.df$GENDER), "other")
# do the filtering
"other" %in% levels(stats20x.df$GENDER) 

# 2. factor levels retain order
levels(stats20x.df$GENDER) <- rev(levels(stats20x.df$GENDER))
# do filtering
all(levels(stats20x.df$GENDER) == c("other", "male", "female"))

###---------------------------------------------------------


# DATASET -> FILTER DATASET -> NUMERIC CONDITION
###---------------------------------------------------------

# example : filtering for column "EXAM" for obs greater than or equal to 80
filtered.EXAM <- stats20x.df %>% 
  dplyr::filter(EXAM >= 80)
###


# nse function
datasetFilterNum <- function(dataset, var, op, value){
  eval <- stringr::str_c(var, op, value, sep = " ")
  dataset %>% 
    dplyr::filter_(eval)
}

# Change these varaibles for the nse function
dataset <- stats20x.df 
var <- "EXAM" 
op <-">=" 
value <- 80

datasetFilterNum.test <- datasetFilterNum(dataset, var, op, value)
###---------------------------------------------------------


# DATASET -> FILTER DATASET -> ROW NUMBER
###---------------------------------------------------------

# example : removing obs with row numbers 1, 2, 3, 4, 6, 7, 8, and 9
filtered.ROW <- stats20x.df %>%
  dplyr::slice((1:n())[-c(1,2,3,4,6,7,8,9)])
# or...
filtered.ROW <- stats20x.df %>% 
  filter(!(row_number() %in% c(1,2,3,4,6,7,8,9)))
###


# nse function
datasetFilterRow <- function(dataset, row_num){
  
  eval <- stringr::str_c("c(", 
                        stringr::str_c((1:nrow(dataset))[-row_num], 
                                       collapse = ", "), 
                        ")", 
                        sep = " ")
  
  dataset %>% 
    dplyr::slice_(eval)
}

# Change these varaibles for the nse function
dataset <- stats20x.df 
row_num <- c(1,2,3,4,6,7,8,9)

datasetFilterRow.test <- datasetFilterRow(dataset, row_num)
###---------------------------------------------------------


# DATASET -> FILTER DATASET -> RANDOMLY
###---------------------------------------------------------

# example : take a 5 random samples each of size 3 without replacement
filtered.RANDOM <- stats20x.df %>%
  dplyr::sample_n(3 * 5, replace = FALSE) %>%
    dplyr::mutate(Sample.Number = rep(1:5, each = 3))
###


datasetFilterRandom <- function(dataset, sample_size, n){
  dataset %>%
    # dplyr::mutate(row.num = 1:nrow(dataset)) %>% # uncomment this to show this is sampling without replacement
    dplyr::sample_n(sample_size * n, replace = FALSE) %>%
      dplyr::mutate(Sample.Number = rep(1:n, each=3))
}

# Change these varaibles for the nse function
dataset <- stats20x.df 
sample_size <- 3
n <- 5

datasetFilterRandom.test <- datasetFilterRandom(dataset, sample_size, n)
###---------------------------------------------------------


###---------------------------------------------------------
# DATASET -> SORT DATA BY VARIABLES

# example : sort data in the following order - "EXAM" (descending), "YEARSSIN" (ascending) and "DEGREE" (descending); NB. categorical variables will be sorted based on their factor levels
sorted.3VARS <- stats20x.df %>% 
  dplyr::arrange(desc(EXAM), YEARSSIN, desc(DEGREE))
### 

 
# nse function
datasetSortVars <- function(dataset, vars, increasing = c(TRUE, TRUE, TRUE, TRUE)){
  eval <- ifelse(increasing, 
                 vars, 
                 stringr::str_c("desc("
                                , vars, 
                                ")"))
  dataset %>% 
    dplyr::arrange_(.dots = as.list(eval))
}

# Change these varaibles for the nse function
dataset <- stats20x.df 
vars <- c("EXAM", "YEARSSIN", "DEGREE")
increasing <- c(FALSE, TRUE, FALSE)

datasetSortVars.test <- datasetSortVars(dataset, vars, increasing)
###---------------------------------------------------------


###---------------------------------------------------------
# DATASET -> AGREEGATE DATA

# example : get the mean, sum and sd for "ASSIGN" and median, iqr and count for "TEST" of the all unique combinations of "PASS" and "GENDER" 
aggregated.2CATS.2NUM <- stats20x.df %>% 
  dplyr::group_by(PASS, GENDER) %>%
    dplyr::summarize(ASSIGN.mean = mean(ASSIGN, na.rm = TRUE), 
                    ASSIGN.sum = sum(ASSIGN, na.rm = TRUE),
                    ASSIGN.sd = sd(ASSIGN, na.rm = TRUE),
                    TEST.median = median(TEST, na.rm = TRUE),
                    TEST.count = n(),
                    TEST.iqr = IQR(TEST, na.rm = TRUE))
###

summarize_vars <- function(.data, vars, 
                           summary = c("mean", "sum", "sd", "median", "count", "iqr"), 
                           return.tidy.code = FALSE) {
  ## ... ## 
}
data %>% summarize_vars(c("ASSIGN", "TEST"), c("mean", "sum", "sd"))

## notes - we'll figure out later. ..
# - keep track of missing counts

dataset_aggregate <- function(dataset, vars, summaries){
  
}

# NEEDS WORK - IQR AND COUNT DON'T WORK AS AT NOW
# NEEDS WORK - disclude NaN's

# Change these varaibles
dataset <- stats20x.df 
vars <- c("PASS", "GENDER")
summaries <- c("sum", "mean", "median", "sd") 

summaries <- sort(as.factor(summaries))
numeric_vars <- colnames(dplyr::select_if(dataset, is.numeric)) %>%
  sort()

numeric_cols <- stringr::str_c(rep(numeric_vars, 
                                   each = length(summaries)), 
                               ".",
                               rep(summaries, 
                                   length(numeric_vars)), 
                               sep= "") 

eval <- numeric_cols %>% 
  stringr::str_c(" = ", 
                 rep(summaries, 
                     length(numeric_vars)), 
                 "(", rep(numeric_vars, 
                          each = length(summaries)), 
                 ", na.rm = TRUE", 
                 ")", 
                 sep = "") 

dataset_aggregate.test <- dataset %>% 
  dplyr::group_by_(.dots = as.list(vars)) %>%
    dplyr::summarise_(.dots = set_names(eval, numeric_cols))
###---------------------------------------------------------


###---------------------------------------------------------
#DATA OPTIONS -> STACK VARIABLES

# example : stack the "EXAM" and "ASSIGN" columns
stacked.2VARS <- stats20x.df %>% 
  tidyr::gather(key = stack.variable, value = stack.value, EXAM, ASSIGN)
###


dataset_stackVars <- function(dataset, stack_var){
  dataset %>% 
    tidyr::gather_(key_col = "stack.variable", 
                   value_col = "stack.value", 
                   gather_cols = stack_var)
}

# Change these variables for the nse function
dataset <- stats20x.df
stack_var <- c("EXAM", "ASSIGN")

dataset_stackVars.test <- dataset_stackVars(dataset, stack_var)
###---------------------------------------------------------


# OLD CODE

# nse function
datasetFilterCat <- function(dataset, var, levels){
  levels <- stringr::str_c('"', levels, '"')
  eval <- stringr::str_c(var, 
                         "%in% c(", 
                         stringr::str_c(levels, 
                                        collapse = ", "), 
                         ")", 
                         sep = " ")
  dataset %>% 
    dplyr::filter_(eval)
}

# Change these varaibles for the nse function
dataset <- stats20x.df
var <- "DEGREE"
levels <- c("BCom", "BA", "Other")

datasetFilterCat.test <- datasetFilterCat(dataset, var, levels)
