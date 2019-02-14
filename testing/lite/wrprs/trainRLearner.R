trainRLearner <- function(task, learner, subset = NULL) {
  
  require(learner$package, character.only = T)
  
  learner.args = list(learner = learner, task = task)
  learner.args$weights = task$weights
  
  if(length(learner$param.user) != 0) {
    
    param.default = lapply(learner$param, "[[", "default")
    names(param.default) = sapply(learner$param, "[[", "id")
    
    param.merged = modifyList(param.default, learner$param.user)
    
    learner.args = c(learner.args, param.merged)
  }
  
  fit = do.call(trainMethod, learner.args)
  
  model = structure(list(task.desc = task$task.desc, 
                         learner = learner, 
                         model.fit = fit),
                    class = c("model", learner$cl))
  
}

###############

trainMethod <- function(learner, task, weights, ...) {
  
  UseMethod("trainMethod")
  
}