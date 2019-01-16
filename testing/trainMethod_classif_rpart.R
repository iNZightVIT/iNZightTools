
trainMethod.classif.rpart <- function(task, learner, ...) {
  
  data = task$env$data
  
  if(is.null(task$weights)) {
    
    formula = task$task.desc$formula
    
    rpart::rpart(formula = formula, data = data, method = "class", ...)
    
  }

}


