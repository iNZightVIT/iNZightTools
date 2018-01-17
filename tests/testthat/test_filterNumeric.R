library(devtools)
library(magrittr)
library(stringr)
library(testthat)


## use the census at school data (this is our "primary example")
# install_github('iNZightVIT/FutureLearnData')  ## -- install this once, directly from github
data('census.at.school.500', package = 'FutureLearnData')
dat <- census.at.school.500 ## - give it a shorter name

# setwd('/path/to/iNZightTools')  ## -- working directory needs to be `iNZightTools` (R studio might do this for you?)

## load package
# - effectively: install.packages('/path/to/iNZightTools'); library(iNZightTools)
load_all() 

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