
# Methods for classif.ctree -------------------------------------------------

learnerMethod.classif.ctree <- function() {
  
  lrn = list(cl = "classif.ctree",
             
             package = "classif.ctree",
             
             name = "C5.0",
             
             short.name = "classif.ctree",
             
             param = structure(m = makeParamSet(makeIntegerLearnerParam(id = "trials", lower = 1L, default = 1L),
                                                makeLogicalLearnerParam(id = "rules", default = FALSE, tunable = FALSE),
                                                makeLogicalLearnerParam(id = "subset", default = FALSE),
                                                # FIXME: Default = 0 throws error because 'lower' = 2L  is above default.
                                                makeIntegerLearnerParam(id = "bands", lower = 2L, upper = 1000L,
                                                                        tunable = FALSE, requires = quote(rules == TRUE)),
                                                makeLogicalLearnerParam(id = "winnow", default = FALSE),
                                                makeLogicalLearnerParam(id = "noGlobalPruning", default = FALSE),
                                                makeNumericLearnerParam(id = "CF", lower = 0, upper = 1, default = 0.25),
                                                # FIXME: upper limit is data dependent
                                                makeIntegerLearnerParam(id = "minCases", lower = 0L, upper = Inf, default = 2L),
                                                makeLogicalLearnerParam(id = "fuzzyThreshold", default = FALSE),
                                                makeNumericLearnerParam(id = "sample", lower = 0, upper = .999, default = 0, tunable = TRUE),
                                                makeIntegerLearnerParam(id = "seed", lower = -Inf, upper = Inf, tunable = FALSE),
                                                makeLogicalLearnerParam(id = "earlyStopping", default = TRUE),
                                                # label just changes the word 'outcome' to something else in the output
                                                makeUntypedLearnerParam(id = "label", default = "outcome", tunable = FALSE)
             ),
             
             class = "param.set"),
             
             param.user = NULL,
             
             callees = c("C5.0", "C5.0Control"))
  
  
  class(lrn) = "classif.ctree"
  
  return(lrn)
  
  
}


# Train -------------------------------------------------------------------


trainMethod.classif.ctree <- function(.task, .learner, .subset = NULL, ...) {
  
  if(!is.null(subset)) {
    
    data <- task$env$data[subset, ]
    
  } else {
    
    data = task$env$data
    
  }
  
  is.prob = learner$predict.type == "probability"
  
  classif.ctree::classif.ctree(dependent.variable.name = task$task.desc$target, data = data, probability = is.prob, weights = task$weights, ...)
  
  
}


# Train -------------------------------------------------------------------

predictMethod.classif.ctree <- function(model, newdata) {
  
  pred = predict(object = model$model.fit, newdata = newdata, type = "response") # what about type = "se"?
  
  return(pred)
  
}
