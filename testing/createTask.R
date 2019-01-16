createTask <- function(data, target, count = F, ordered.vars = NULL, weights.col = NULL, fix.factor.levels = T) { # If task type is surv, target is a named list.
  
  # Encode ordered factors
  data[ordered.vars] <- lapply(data[ordered.vars],
                               function(x) if(!is.ordered(x)) factor(x, ordered = T) else x)
  
  if(!"time" %in% names(target)) { # This check depends on what teh GUI will look like
    
    x = data[[target]]
    
    if(is.factor(x)) {
      data[[target]] = droplevels(x)
      task.type = "classif"
    }
    
    if(is.logical(x)) {
      data[[target]] = as.factor(x)
      task.type = "classif"
    }
    
    if(is.integer(x)) { # Would it be more correct to do is.integer(x) || is.numeric(c)?
      data[[target]] = as.double(x)
      task.type = "regr"
    }
    
  } else {
    
    time = data[[target$time]]
    event = data[[target$event]]
    
    if (is.integer(time)) data[[target$time]] = as.double(time)
    
    # Converting event column to logical 
    if (is.numeric(event)) {
      if (all(as.integer(event) %in% c(0L, 1L))) 
        data[[target$event]] = (as.integer(event) == 1L)
    }
    
    else if (is.factor(event)) {
      lvls = levels(event)
      if (length(lvls) == 2L) {
        if (all(lvls %in% c("TRUE", "FALSE"))) {
          data[[target$event]] = (event == "TRUE")
        }
        else if (all(lvls %in% c("0", "1"))) {
          data[[target$event]] = (as.character(event) == "1")
        }
      }
    } # What if the event levels are something other than 0/1 or T/F? Should the user be asked to specify the event name? 
    
  }
  
  # Check validity of weights
  if(!is.null(weights.col)) {
    
    wts = data[[weights.col]]
    
    if(length(wts) != nrow(data)) stop("Number of weights must be the same as the number of observations.") # Improve wording
    if(sum(wts) != 1) stop("Weights must sum to 1.")
    if(any(wts) < 0) stop("Weights cannot be less than 0.")
    
  } else {
    
    wts = NULL
    
  }
  
  if(fix.factor.levels) {
    
    data = droplevels(data, except = which(!names(data) %in% target)) # Check this - in mlr fix.factors.prediction is a makeLearner arg
    
  }
  
  if(task.type == "surv") {
    
    resp = sprintf('survival::Surv(%s, %s, type = "right")', time, event)
    
  } else {
    
    resp = target
  }
  
  formula = as.formula(paste(resp, " ~ ."), env = parent.frame())
  
  
  # Figuring out the number of features of each class
  feat.names = setdiff(names(data), c(target, weights))
  
  col.class = lapply(iris, function(x) class(x)[1L])
  feat.class =  col.class[setdiff(names(col.class), c(target, weights))]
  
  n.feat = c(total = length(feat.names),
             numerics = sum(feat.class == "numeric"),
             factors = sum(feat.class == "factor"), 
             ordered = sum(feat.class == "ordered"))
  
  env = new.env(parent = emptyenv())
  env$data = data
  
  # Creating task object
  task = list(task.type = task.type,
              data.name = deparse(substitute(data)),
              env = env,
              weights = wts,
              task.desc = list(target = target,
                               features = feat.names,
                               formula = formula,
                               n.obs = nrow(data), 
                               n.feat = n.feat, 
                               has.missings = is.na(data),
                               has.weights = !is.null(weights)))
  
  if(task.type == "classif") {
    
    task$task.desc$class.levels = levels(data[[target]])
    task$task.desc$class.dist = table(data[[target]])
    
  }
  
  class(task) <- "task"
  
  return(task)
  
}






# # why are these the same, c() has unlisting effect
# 
# setdiff(names(iris), c(unlist(tars), "right"))
# setdiff(names(iris), c(tars, "right"))
# 
# 
# # Why is new.env(parent = emptyenv()) being used?
# env = new.env(parent = emptyenv())
# env$data = data
# makeS3Obj("Task", type = type, env = env, weights = weights, 
#           blocking = blocking, coordinates = coordinates, task.desc = NA)