trainMethod.classif.rpart <- function(task, learner, subset = NULL, ...) {
  
  if(!is.null(subset)) {
    
    data <- task$env$data[subset, , drop = F]
    
  } else {
    
    data = task$env$data
    
  }
  
  
  if(!is.null(task$weights)) {
    
    weights = task$weights
    
    formula = task$task.desc$formula
    
    rpart::rpart(formula = formula, data = data, weights = weights, method = "class", ...)
    
  } else {
    
    rpart::rpart(formula = formula, data = data, method = "class", ...)
    
  }
  
}
