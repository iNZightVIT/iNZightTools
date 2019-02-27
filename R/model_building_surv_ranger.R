


# Methods for regr.ranger -------------------------------------------------

learnerMethod.regr.ranger <- function() {
  
  lrn = list(cl = "regr.ranger",
             
             package = "ranger",
             
             name = "Random Forest",
             
             short.name = "ranger",
             
             param = structure(m = makeParamSet(makeIntegerParam(id = "num.trees", default = 500L, lower = 0L, tunable = F), # doesn't make sense to tune num.trees - the more the better
                                                makeIntegerParam(id = "mtry", default = , lower = 0, upper = ),
                                                #makeIntegerParam(id = "min.node.size", default = switch(predict.type, "class" = 1L, "prob" = 10L), lower = 1L, upper = ),
                                                makeLogicalParam(id = "replace",default = T), 
                                                makeLogicalParam(id = "respect.unordered.factors", default = T, tunable = F),
                                                makeNumericParam(id = "sample.fraction", lower = 0L, upper = 1L), 
                                                makeDiscreteParam(id = "splitrule", values = c("variance", "extratrees", "maxstat"), default = "variance"),
                                                makeIntegerParam(id = "num.random.splits", lower = 1L, default = 1L, requires = quote(splitrule == "extratrees")),
                                                makeDiscreteLearnerParam(id = "importance", values = c("none", "impurity", "permutation"), default = "none", tunable = FALSE),
                                                makeLogicalParam(id = "scale.permutation.importance", default = FALSE, requires = quote(importance == "permutation"), tunable = FALSE)
             ),
             
             class = "param.set"),
             
             param.user = NULL,
             
             callees = c("ranger"))
  
  
  class(lrn) = "regr.ranger"
  
  return(lrn)
  
  
}


# Train -------------------------------------------------------------------


trainMethod.regr.ranger <- function(.task, .learner, .subset = NULL, ...) {
  
  if(!is.null(subset)) {
    
    data <- task$env$data[subset, ]
    
  } else {
    
    data = task$env$data
    
  }
  
  ranger::ranger(dependent.variable.name = task$task.desc$target[["time"]], 
                 status.variable.name = task$task.desc$target[["status"]],
                 data = data, 
                 weights = weights, ...) # passed target name instead of formula since formula interface is slow (see ranger man p.17)
      
  
}


# Train -------------------------------------------------------------------

predictMethod.regr.ranger <- function(model, newdata) {
  
  pred = predict(object = model$model.fit, newdata = newdata, type = "response") 
  
  return(pred)
  
}

