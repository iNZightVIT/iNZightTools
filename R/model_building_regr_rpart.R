

# Methods for regr.rpart --------------------------------------------------

learnerMethod.regr.rpart <- function() {
  
  lrn = list(cl = "regr.rpart",
             
             package = "rpart",
             
             name = "CART Decision Tree",
             
             short.name = "rpart",
             
             param = structure(makeParamSet(makeIntegerParam(id = "minsplit", default = 20L, lower = 1L),
                                            #makeIntegerParam(id = "minbucket", default = round(minsplit/3), lower = 1L),
                                            makeNumericParam(id = "cp", default = 0.001, lower = 0, upper = 1),
                                            makeIntegerParam(id = "maxdepth", default = 30L, lower = 1L, upper = 30L), # mlr uses 30 as upper limit, rpart.control ____
                                            makeIntegerParam(id = "xval", default = 10L, lower = 0L, tunable = F),
                                            makeIntegerParam(id = "maxcompete", default = 4L, lower = 0L),
                                            makeIntegerParam(id = "maxsurrogate", default = 5L, lower = 0L),
                                            makeDiscreteParam(id = "usesurrogate", default = 2L, values = 0:2),
                                            makeDiscreteParam(id = "surrogatestyle", default = 0L, values = 0:1)
                                            ),
                               
                               class = "param.set"),
             
             param.user = NULL,
             
             callees = c("rpart", "rpart.control"))
  
  
  class(lrn) = "regr.rpart"
  
  return(lrn)
  
  
}


# Train -------------------------------------------------------------------


trainMethod.regr.rpart <- function(.task, .learner, .subset = NULL, ...) {
  
  if(!is.null(subset)) {
    
    data <- task$env$data[subset, ]
    
  } else {
    
    data = task$env$data
    
  }
  
  formula = task$task.desc$formula
  
  if(task$task.desc$target.is.count) {
    
    method = "poisson"
    
  } else {
    
    method = "anova"
    
  }
  
  
  if(!is.null(task$weights)) {
    
    weights = task$weights
    
    rpart::rpart(formula = formula, data = data, weights = weights, method = method, ...)
    
  } else {
    
    rpart::rpart(formula = formula, data = data, method = method, ...)
    
  }
  
}


# Predict -----------------------------------------------------------------


predictMethod.regr.rpart <- function(model, newdata) {
  
  pred = predict(object = model$model.fit, newdata = newdata, type = "vector")
  
  return(pred)
  
}

