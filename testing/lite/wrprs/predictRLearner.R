predictRLearner <- function(model, task, newdata, subset = NULL) {
  
  if(!xor(missing(task), missing(newdata))) { # if((missing(task) & missing(newdata)) | (!is.null(task) & !is.null(newdata))) would give Error in predictRLearner(mod, task) : argument "newdata" is missing, with no default
    
    
    stop("Must specify one of `task` or `new data`, but not both.")
    
  } 
  
  
  if(missing(newdata)) {
    
    newdata = task$env$data
    
  } else{
    
    newdata = newdata
  }
  
  
  if(!is.null(subset)) {
    
    newdata = newdata[subset, , drop = F]
    
  } else {
    
    newdata = newdata
    
  }
  
  
  pred.args = list(model, newdata = newdata)
  
  pred = do.call(predictMethod, pred.args)
  
  return(pred)
  
}

###########################

predictMethod <- function(model, newdata) {
  
  UseMethod("predictMethod")
  
}
