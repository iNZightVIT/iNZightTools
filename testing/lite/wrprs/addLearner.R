addLearner <- function(task, learner.name,
                          predict.type = c("response", "probability"), # predict.type is dynamic in the GUI, options (c("response", "probability") for classif/surv tasks OR just "response" for regr tasks) depend on task.type
                          param.user = list()) { # param.user is populated in the GUI, list of pars available to user is dynamic (depends on the learner they choose)
  
  cl = sprintf("%s.%s", task$task.desc$task.type, learner.name)
  
  learner.method = getS3method("learnerMethod", class = cl)
  
  # Get default param settings for learner
  learner = do.call(learner.method, list())
  
  learner$param.user = list()
  
  # Check user defined para vals are valid
  if(length(param.user) != 0) {
    
    param = learner$param
    param.names = names(param.user)
    
    for(i in seq_along(param.user)) {
      
      n = param.names[i]
      p = param.user[[i]]
      pd = param[[n]]
      
      if(p < pd$lower || p > pd$upper) {
        
        stop(sprintf("Parameter `%s` must be in range [%f, %f]", n, pd$lower, pd$upper))
        
      } else {
        
        learner$param.user[[n]] = p
        
      }
      
    }
    
  } else {
    
    learner$param.user = param.user
    
  } # If the user does not override any defaults, set learner$par.vals to the empty list par.vals = list()
  
  learner$predict.type = predict.type
  
  class(learner) = c("learner", class(learner))
  
  return(learner)
  
}

############################################

learnerMethod <- function() {
  
  UseMethod("learnerMethod")
  
}

