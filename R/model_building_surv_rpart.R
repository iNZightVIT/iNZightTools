


# Methods for surv.rpart --------------------------------------------------

learnerMethod.surv.rpart <- function() {
  
  lrn = list(cl = "surv.rpart",
             
             package = "rpart",
             
             name = "CART Decision Tree",
             
             short.name = "rpart",
             
             param = structure(makeParamSet(makeIntegerParam(id = "minsplit", default = 20L, lower = 1L),
                                            #makeIntegerParam(id = "minbucket", default = round(minsplit/3), lower = 1L),
                                            makeNumericParam(id = "cp", default = 0.001, lower = 0, upper = 1),
                                            makeIntegerParam(id = "maxdepth", default = 30L, lower = 1L, upper = 30L), # rpart man (p.23): "Values greater than 30 rpart will give nonsense results on 32-bit machines."
                                            makeIntegerParam(id = "xval", default = 10L, lower = 0L, tunable = F),
                                            makeIntegerParam(id = "maxcompete", default = 4L, lower = 0L),
                                            makeIntegerParam(id = "maxsurrogate", default = 5L, lower = 0L),
                                            makeDiscreteParam(id = "usesurrogate", default = 2L, values = 0:2),
                                            makeDiscreteParam(id = "surrogatestyle", default = 0L, values = 0:1),
                                            makeNumericParam(id = "parms", default = 1, lower = 0) # coefficient of variation - check if lower bound is correct
                                            ),
                               
                               class = "param.set"),
             
             param.user = NULL,
             
             callees = c("rpart", "rpart.control"))
  
  
  class(lrn) = "surv.rpart"
  
  return(lrn)
  
}



# Train -------------------------------------------------------------------


trainMethod.surv.rpart <- function(.task, .learner, .subset = NULL, ...) {
  
  if(!is.null(.subset)) {
    
    data <- .task$env$data[.subset, ]
    
  } else {
    
    data = .task$env$data
    
  }
  
  formula = .task$task.desc$formula
  
  if(!is.null(.task$weights)) {
    
    weights = .task$weights
    
    rpart::rpart(formula = formula, data = data, weights = weights, method = "exp", ...)
    
  } else {
    
    rpart::rpart(formula = formula, data = data, method = "exp", ...)
    
  }
  
}



# Predict -----------------------------------------------------------------

predictMethod.surv.rpart <- function(model, newdata) {
  
  pred = predict(object = model$model.fit, newdata = newdata, type = "vector")
  
  return(pred)
  
}

