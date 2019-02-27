# Add roxygen


# Create task -------------------------------------------------------------

createTask <- function(id = deparse(substitute(data)), data, target, predictors,
                       count = F, ordered.vars = NULL, weights = NULL) { 
  
  id = force(id)
  
  # Encode ordered factors
  data[ordered.vars] <- lapply(data[ordered.vars],
                               function(x) if(!is.ordered(x)) factor(x, ordered = T) else x)
  
  if("time" %in% names(target)) { # This check depends on what the GUI will look like
    
    task.type = "surv"
    
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
    
  } else {
    
    x = data[[target]]
    
    if(is.factor(x)) {
      #data[[target]] = droplevels(x) # Q: Not sure if dropping unused levels in the target is needed - mlr's makeClassifTask uses it
      task.type = "classif"
    }
    
    if(is.logical(x) || is.character(x)) {
      data[[target]] = as.factor(x)
      task.type = "classif"
    }
    
    if(is.integer(x) || is.numeric(x)) { # Would it be more correct to do is.integer(x) || is.numeric(c)?
      data[[target]] = as.double(x)
      task.type = "regr"
    }
    
  }
  
  # Check validity of weights
  if(!is.null(weights)) {
    
    if(length(weights) != nrow(data)) stop("Number of weights must be the same as the number of observations.") # Improve wording
    if(sum(weights) != 1) stop("Weights must sum to 1.")
    if(any(weights) < 0) stop("Weights cannot be less than 0.")
    
  } else {
    
    wts = NULL
    
  }

  
  if(task.type == "surv") {
    
    resp = sprintf('survival::Surv(%s, %s, type = "right")', time, event)
    
  } else {
    
    resp = target
  }
  
  formula = as.formula(paste(resp, " ~ ."), env = parent.frame())
  
  # features by type
  
  # col.cl = lapply(data, class)
  # 
  # feats = split(rep(names(col.cl), sapply(col.cl, length)), unlist(col.cl))
  
  col.cl = lapply(data, function(x) class(x)[1])
  
  feats = split(names(col.cl), unlist(col.cl))
  
  feats$all = predictors
  
  # feature count by type
  n.feats = lapply(feats, length)
  
  env = new.env(parent = emptyenv())
  env$data = data[c(target, feats$all)]
  
  # Creating task object
  task = list(id = id, 
              env = env,
              weights = wts,
              fix.factor.levels = fix.factor.levels,
              task.desc = list(
                task.type = task.type,
                target = target,
                feats = feats, # Won't make sense to print this as is if user inputs high dim data.
                formula = formula,
                n.obs = nrow(data),
                n.feats = n.feats,
                target.is.count = count,
                complete.cases = sum(complete.cases(data)),
                has.missings = any(is.na(data)),
                has.weights = !is.null(weights)))
  
  if(task.type == "classif") {
    
    task$task.desc$class.levels = levels(data[[target]])
    task$task.desc$class.dist = table(data[[target]])
    
  }
  
  class(task) <- c(task$task.desc$task.type, "task")
  
  return(task)
  
}



# Add learner -------------------------------------------------------------

addLearner <- function(task, learner.name, param.user = list(), 
                       predict.type = "response", fix.factor.levels = T) { # options for predict.type depend on learner and task type
  
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


learnerMethod <- function() {
  
  UseMethod("learnerMethod")
  
}



# Train learner -----------------------------------------------------------

trainRLearner <- function(task, learner, subset = NULL) {
  
  require(learner$package, character.only = T)
  
  learner.args = list(.learner = learner, .task = task, .subset = NULL)
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


trainMethod <- function(.learner, .task, .weights, ...) {
  
  UseMethod("trainMethod")
  
}



# Predict -----------------------------------------------------------------

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
    
    newdata = newdata[subset, ]
    
  } else {
    
    newdata = newdata
    
  }
  
  pred.args = list(model = model, newdata = newdata)
  
  pred = do.call(predictMethod, pred.args)
  
  t.col <- grep(task$task.desc$target, colnames(newdata))
  
  pred = list(pred = pred, 
              truth = newdata[, t.col, drop = T])
  
  return(pred)
  
}


predictMethod <- function(model, newdata) {
  
  UseMethod("predictMethod")
  
}


# Benchmarking ------------------------------------------------------------


bmParallel <- function(learners, task, resampling, measures) {
  
  rdesc = do.call(makeResampleDesc, resampling)
  
  id = task$id
  
  args = list(id = id, 
              data = task$env$data, 
              target = task$task.desc$target, 
              weights = task$weights)
  
  task.fun <- get(paste0("make", gsub("(^[a-z])", "\\U\\1", task$task.desc$task.type, pe = T), "Task")) # need to modify for surv tasks: event and time
  
  task.mlr = do.call(task.fun, args) 
  
  rin = makeResampleInstance(desc = rdesc, task = task.mlr) # there is a size arg that can be used in place of task but stratification requires the task.  
  
  sn = lapply(learners, "[[", "short.name")
  
  # args <- as.list(match.call(expand.dots = F))[-1] why didn't this work?
  
  # Using parallelisation to speed up computation
  parallelStart()
  
  results = parallelMap(doResampling, learners, more.args = list(task = task, 
                                                                 rin = rin, 
                                                                 measures = measures))
  
  results = setNames(results, sprintf("learner%d.%s", seq_along(learners), sn))
  
  
  parallelStop()
  
  bmr = list(results = results, 
             learners = learners,
             measures = measures)
  
  class(bmr) <- "bmrResult"
  
  return(bmr)
  
}


# Resampling --------------------------------------------------------------


doResampling <- function(learner, task, rin, measures) {  # does each iteration need a class?
  
  iters <- seq_len(rin$desc$iters)
  
  res = parallelMap(doIteration, iters, more.args = list(learner = learner, 
                                                         task = task,
                                                         rin = rin,
                                                         measures = measures))
  class(res) <- "resampleResult"
  
  return(res)
  
}


doIteration <- function(iter, learner, task, rin, measures) {
  
  data <- task$env$data
  
  train.idx = rin$train.inds[[iter]]
  test.idx = rin$test.inds[[iter]]
  
  mod = trainRLearner(task, learner, subset = train.idx)
  
  pred = predictRLearner(mod, task, subset = test.idx) #should I add a subset argument instead? Also, what's the point in predicting both the test and training sets?
  
  # looping over performance measures for learner i
  perf = lapply(measures, measurePerf, x = pred)
  
  perf = setNames(perf, measures)
  
  iter.res = list(model = mod,
                  pred.test = pred,
                  perf.test = perf)
  
  return(iter.res)
  
}


# Performance -------------------------------------------------------------

measurePerf <- function(x, measure) {
  
  pred <- x$pred
  
  truth <- x$truth
  
  switch(measure,
         
         "mmce" = {
           
           mean(pred != truth)
           
           },
         
         "ber" = {
           
           conf.mat = table(truth, pred)
           mean(1 - diag(conf.mat)/rowSums(conf.mat))
           
           },
         
         "rmse" = {
           
           pred
           
           },
         
         stop("Measure not found.")
         
         )
}



# Benchmark result --------------------------------------------------------

as.data.frame.bmResult <- function(bmr) {
  
  res = bmr$results
  
  df = map_df(res, ~ .x %>%
                map(pluck, "perf.test")  %>%
                melt(value.name = "perf") %>%
                dplyr::rename(measure = "L2", iter = "L1"), .id = "learner")
  
  return(df)
  
}


bmBoxplot <- function(bmdf) {
  
  p = bmdf %>% mutate(measure = ifelse(measure == "rsq", "R-squared", toupper(measure))) %>% # will have to reformat learner col too - possibly print settings that differ from default under each label
    group_by(measure) %>% # Am I grouping on the right var? I don't think grouping is required here. 
    arrange(perf) %>% # For some measures (e.g. rsq), we need to use arrange(desc())
    ggplot(aes(x = learner, y = perf, 
               colour = learner, 
               fill = learner)) +
    geom_boxplot(alpha = 0.45) +
    theme(strip.text.x = element_text(size=9, color="black", face="bold"), 
          axis.text = element_text(size=4),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank()) +
    facet_wrap(~measure) + 
    labs(title = "CV Performance") + 
    coord_flip() +
    scale_fill_discrete_qualitative(name = "Learner") +
    scale_color_discrete_qualitative(guide = F)  
  
  p = ggplotly(p)
  
  return(p)  
  
}


# Misc --------------------------------------------------------------------

stratify.cols.choices <- function(task, resampling) { 
  
  # retrieve factor columns (mlr supports stratification of integer columns as well - not sure if this is necessary so I've just stuck with factors for now). 
  fct.names <- task$task.desc$feats$factor
  
  fct.cols <- task$env$data[fct.names]
  
  # remove NAs
  fct.cols <- na.omit(fct.cols) # is this correct? or should I leave NAs in?
  
  test.prop = switch(resampling$method, 
                     "Holdout" = resampling$split, 
                     "CV" = 1/resampling$iters, 
                     "RepCV" = 1/resampling$folds)
  
  strat.cols = lapply(fct.cols, function(x) all(table(x)*test.prop >= 1)) # check this
  
  enabled = fct.names[which(unlist(strat.cols))]
  
  disabled = !(fct.names %in% enabled)
  
  choices = list(enabled, disabled)
  
  return(choices)
}









