required <- list("rpart", "ranger", "randomForest", "maptree", "cluster", "icebox", "ggplot2", "plotly")
new.pkgs <- required[!(required %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.pkgs)

install.packages(c("rpart", "ranger", "randomForest", "maptree", "cluster", "icebox", "ggplot2", "plotly"))

# Load packages
lapply(required, require, character.only = TRUE)

################################
# Prelim checks
################################

super <- function(y, train, test = NULL, time = NULL, event = NULL, ordered.fct = NULL, ...) {
  
  # Don't need the check below 
  # df = if(is.data.frame(df)) df else as.data.frame(df) 
  
  # Ensure ordered factors are encoded accordingly.
  # This will speed up computation for rpart (and ranger) - see last sentence of pg. 29 of the rpart 'long intro'.
  train[ordered.fct] <- lapply(train[ordered.fct],
                               function(x) if(!is.ordered(x)) factor(x, ordered = T) else x)
  
  train[ordered.fct] <- lapply(test[ordered.fct],
                               function(x) if(!is.ordered(x)) factor(x, ordered = T) else x)
  
  
}

rpart::rpart(y = iris$Species, x = iris[,-5])

################################
# CART 
################################

wrapperCART <- function(y, train, test = NULL, count = F, time = NULL, 
                        cp = .001, xval = 10L, prior = NULL, loss = NULL, split = "gini", ...) {
  
  # Get values of all args in call - formals returns the defaults, match.call returns the user-defined values 
  # modifyList merges the two lists so that default values are overwritten by the user-specified values
  allArgs <- modifyList(formals(), as.list(match.call(expand.dots = TRUE)))
  allArgs <- allArgs[names(allArgs) != "..."]
  
  
  is.surv = is.factor(train[y]) && !is.null(time)
  
  xvars <- colnames(train)[ ! names(train) %in% c(y, time) ]
  
  resp <- if (is.survtime) {
    
    sprintf("survival::Surv(%s, %s, type = \"right\")", train[time], train[y]) # NOTE: type = "right" censored by default.
    
  } else  train[y] 
  
  fo <- as.formula(sprintf("%s ~ %s", resp, paste(xvars, collapse = "+")), env = parent.frame())
  
  # I could leave it up to rpart to infer the method from the column class but it cannot pick up on count data. 
  if(is.factor(train[y])) {
    
    method = "class"
    param = allArgs[c("prior", "loss", "split")]
    
  } else if(is.numeric(train[y])) {
    
    method = "anova"
    param = NULL
    
  } else if(is.surv) {
    
    method = "exp" # Documentation says "If y is a survival object, then method = "exp" is assumed, if y has 2 columns then method = "poisson" - not sure what '2 columns' means.
    # param = ?
    
  }
  
  ctl <- do.call("rpart.control", allArgs[formalArgs(rpart.control)])
  
  # Build tree. Remember to add support for ctree (conditional inference trees)
  fit = rpart::rpart(formula = fo, 
                     data = train, 
                     method = method,
                     parms = param, 
                     control = ctl, 
                     ...)
  
  # Prune tree
  min.cp <- fit$cptable[which.min(fit$cptable["xerror"]), "CP"] # identify sub-tree with min CP
  fit.pruned <- prune(fit, min.cp)
  
  # Performance summary
  fit.pruned
  if(!is.null(test)) {
    error = testError(fit.pruned)
    
  } else error = NULL
  
  rpart.summary = structure(list(label = "CART",
                                 learner = list(),
                                 learner.model = list(),
                                 task.desc = list(),
                                 pred = ,
                                 measures.train = ,
                                 measures.test = 
                                   package = if(is.surv) c("rpart", "survival") else c("rpart"),
                                 callees = c("rpart", "rpart.control"),
                                 type = switch(class(train[y]), 
                                               "factor" = if(is.surv) "Survival" else "Classification", 
                                               "numeric" = "Regression"), 
                                 
  ),
  class = c("inz.tree.rpart", "inz.tree", "inz.mbuilder"))
  
  return(rpart.summary)
}

data(iris)
class(iris$Species)

                            
wrpr <- function(y, train, test, n.iter = 1000, ...) {
  ## first deal with the user config settings etc.
  z <- something_to_do_with_settings(y, ...)
  z <- fit_model(z, n.iter = n.iter, n.core)
  if (!missing(test)) z <- fit_tests(z, test = test)
  z
}

library(rpart)
data(car90)
b = wrapperCART(train = car90, y = "Price", type = "regr")
                            
                            
## assuming the testing code is all up and running ... 
b = fit_tests(b, test)

## would be exactly equivalent to
b = wrapperCART(train = car90, y = "Price", type = "regr", test = test)

# Comparing like with like

################################
# Random Forest 
################################

# Eventually expand the support to include cforest
# NOTE!!! ranger does not handle missing data! Neither does the next best option (randomForest, Rborist, randomForetSRC) - these must be imputed extrnally or omitted.
# Alternative would be to use c4.5 trees instead of CART.

wrapperRF <- function(type, y, train, y.test = NULL, test = NULL, tune = F, ordered.vars = NULL, 
                      probability = F,
                      num.trees = 500, 
                      mtry = NULL,
                      importance = "impurity_corrected",
                      quantreg = F,
                      num.threads = as.numeric(Sys.getenv("NUMBER_OF_PROCESSORS")), 
                      ...) {
  z <- fit_model(z, n.iter = n.iter, n.core)
  train[ordered.vars] <- lapply(train[ordered.vars],
                                function(x) if(!is.ordered(x)) factor(x, ordered = T) else x)
  
  if(type == "surv") {
    
    statvar = y[[1]] 
    if("time" %in% y) depvar = y[["time"]] else depvar = NULL
    
  } else {
    
    depvar = y[[1]]
    statvar = NULL
    
  }
  
  if(is.null(mtry)) {
    
    ncol <- ncol(train) # Proxy for number of predictors (not perfect)
    
    mtry = floor(switch(type,
                        "classif" = sqrt(ncol - 1),
                        "regr" = (ncol - 1)/3,
                        "surv" = if("time" %in% y) sqrt(ncol - 2) else sqrt(ncol - 1)))
    
  } else mtry = floor(mtry)
  
  if(!tune) {
    
    fit <- ranger(data = train,
                  dependent.variable.name = depvar,
                  status.variable.name = statvar,
                  probability = probability,
                  importance = importance, scale.permutation.importance = F,
                  num.trees = num.trees,
                  mtry = mtry,
                  quantreg = F,
                  num.threads = num.threads,
                  respect.unordered.factors = T,
                  write.forest = T,
                  ...)
    
  } else {
    
    task.funs = c("makeClassifTask", "makeRegrTask") # tuneRanger does not appear
    
    task = do.call(tasks[grepl("regr", task.funs, ignore.case = T)], list(data = train, target = y[[1]]))
    
    estimateTimeTuneRanger(iris.task) # Estimated runtime to be displayed to the user
    
    allArgs <- c(mget(names(formals()), sys.frame(sys.nframe())), list(...))
    
    # Tune mtry and sampling scheme (sample size and replacement)
    res = tuneRanger(task, 
                     parameters = allArgs[formalArgs(ranger)],
                     tune.parameters = c("mtry", "replace", "sample.fraction"), 
                     num.trees = num.trees,
                     num.threads = num.threads, 
                     iters = 70, iters.warmup = 30)
  }
  
  #Performance summary  
  if(!is.null(test)) {
    
    testError(model = fit, y.test = y.test, test = test)
    
  }
  
  rpart.summary = structure(list(model = "sdf",
  )
  class = c("inz.tree.rf", "inz.tree", "inz.mbuilder"))
  
  return(rf.summary)
  
}

train.idx <- sample(nrow(iris), 2/3 * nrow(iris))
iris.train <- iris[train.idx, ]
iris.test <- iris[-train.idx, ]

a = wrapper.rf(type = "classif", y = "Species", train = iris.train)
print(a)



#https://rviews.rstudio.com/2017/03/01/the-r-formula-method-the-bad-parts/

#http://tagteam.harvard.edu/hub_feeds/1981/feed_items/2177436



testError <- function(model, y.test, test, ...) {
  
  browser()
  true = test$y.test[[1]]
  
  pred <- predict(fit, data = test)$predictions
  
  if(type %in% c("classif", "surv")) {
    
    conf.mat =  table(true, pred)
    
    test.acc = sum(diag(conf.mat)) / sum(conf.matrix)
    
    test.error = 1 - test.acc 
    
    error = list(conf.mat, test.acc, test.error)
    
    if(probability) {
      
      #test.error = log-loss or Brier (leaning towards log loss)
      #Need to ask Prof Lumley if it is ok to use the same measure that the model was optimising
      
    }
    
  } else {
    
    test.mse = sum((pred - true)^2, na.rm = TRUE) / sum(!is.na(pred))
    
    v = sum((true - mean(true))^2) / length(true) # Using sum of squared differences divided by n instead of variance
    
    test.rsq = 1 - test.mse/v 
    
    error = list(test.mse, test.rsq)
    
  } 
  
}



# S3 classes
fit_model <- function(x, ...) UseMethod("fit_model")

fit_model.inz.tree.rpart <- function(x, n.iter = 1000, ...) {
  ## fit the rpart model
  config <- x$config
  config$n.iter = n.iter
  
  do.call(rpart::rpart, config)
}






