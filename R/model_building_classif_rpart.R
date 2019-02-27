 

# Methods for classif.rpart -----------------------------------------------

learnerMethod.classif.rpart <- function() {
  
  lrn = list(cl = "classif.rpart",
             
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
                                            makeDiscreteParam(id = "surrogatestyle", default = 0L, values = 0:1),
                                            makeDiscreteParam(id = "split", default = "gini", values = list("gini", "information"))
                                            #makeVectorParam(id = "prior")
                                            #makeVectorParam(id = "loss") - loss matrix - not sure what this would look like in the ui
                                            ),
                               
                               class = "param.set"),
             
             param.user = NULL,
             
             callees = c("rpart", "rpart.control"))
  
  
  class(lrn) = "classif.rpart"
  
  return(lrn)
  
}



# Train -------------------------------------------------------------------

trainMethod.classif.rpart <- function(.task, .learner, .subset = NULL, ...) {
  
  if(!is.null(.subset)) {
    
    data <- .task$env$data[.subset, ]
    
  } else {
    
    data = .task$env$data
    
  }
  
  formula = .task$task.desc$formula
  
  dots = list(...)
  
  parms = c("split")
  
  
  if(!is.null(.task$weights)) {
    
    weights = .task$weights
    
    rpart::rpart(formula, data, weights, method = "class", parms = dots[parms], dots[-parms])
    
  } else {
    
    rpart::rpart(formula, data, method = "class", parms = dots[parms], dots[-parms])
    
  }
  
}



# Predict -----------------------------------------------------------------

predictMethod.classif.rpart <- function(model, newdata) {
  
  type = switch(model$learner$predict.type, 
                response = "class", 
                probability = "prob")
  
  
  pred = predict(object = model$model.fit, newdata = newdata, type = type)
  
  return(pred)
  
}

