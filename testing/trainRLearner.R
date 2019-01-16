trainRLearner <- function(task, learner) {
  
  require(learner$package, character.only = T)
  
  if(length(learner$param.user) != 0) {
    
    param.default = lapply(learner$param, "[[", "default")
    names(param.default) = sapply(learner$param, "[[", "id")
    
    param.merged = modifyList(param.default, learner$param.user)
    
  }
  
  learner.args = list(learner = learner, task = task)
  learner.args$weights = task$weights
  learner.args = c(learner.args, param.merged)
  
  fit = do.call(trainRLearner, learner.args)
  
  model = structure(list(task.desc = task$task.desc, 
                         learner = learner, 
                         model.fit = fit),
                    class = "model")
  
}

learner = learner.rpart
task = task.iris

###############

trainMethod <- function(learner, task, weights, ...) {
  
  UseMethod("trainMethod")
  
}
