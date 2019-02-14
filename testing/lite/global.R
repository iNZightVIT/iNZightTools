require(shiny)
require(shinydashboard)
require(shinyjs)
require(DT)
require(corrplot)
require(shinyWidgets)
require(shinyBS)
require(shinyFeedback)
require(mlr)
require(tidyverse)
require(parallelMap)

# Sourcing wrappers - might not need to source the methods  
wrprs <- list.files("./wrprs", pattern = "\\.[rR]$", full.names = T) 
lapply(wrprs, source) 


rmarkdown::render("./wrprs/createTask.R", output_dir = "./www")


# Load learners

getLearners <- function(task) {

  task.type = task$task.desc$task.type

  methodNames <- strsplit(as.character(methods("learnerMethod")), " ")

  methodNames <- methodNames[grep(as.character(task.type), methodNames)]

  learnerNames = gsub(".+\\.(.*)", "\\1", methodNames)

  learners = lapply(learnerNames, function(x) addLearner(task, x))

  return(learners)
}


# Function to fix menuItem/associated tab toggle
convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}


datasets = list(
  'iris' = iris, 
  'mtcars' = mtcars) # What is the difference between this and list(iris = iris, mtcars = mtcars)












######################## Might not need this stuff

# # Getting learner names 
# learner.lines = lapply(learners, readLines)
# name.line = sapply(learner.lines, function(x) grep(" name =", x))
# learner.names = mapply(function(x, y) x[y], learner.lines, name.line)
# learner.names = gsub("^.+\"(.+)\".*$", "\\1", learner.names)

# names(learners) <- learner.names

# data(mtcars)
# mtcars2 = mtcars[, c('hp', 'mpg')]

# shinyApp(
#   ui = dashboardPage(
#     header,
#     sidebar,
#     body, shinyjs::useShinyjs()
#   ),
#   server = server
# )
