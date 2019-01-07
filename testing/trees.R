required <- list("rpart", "ranger", "randomForest", "maptree", "cluster", "icebox", "ggplot2", "plotly")
new.pkgs <- required[!(required %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.pkgs)

install.packages(c("rpart", "ranger", "randomForest", "maptree", "cluster", "icebox", "ggplot2", "plotly"))

# Load packages
lapply(required, require, character.only = TRUE)

################################
# CHECKS 
################################

checks <- function(type, y, train, test = NULL, ...) {
  
  train = if(is.data.frame(train)) train else as.data.frame(train) 
  
  if(type == "classif") {
    
    train[ycol] <- as.factor[train[ycol]]
    
  } else if(type == "surv"){
    
    train[ycol] <- train[ycol] == y[["success"]]
    train[y[["time"]]] <- as.numeric(train[y[["time"]]])
    
  } else{
    
    train[ycol] <- as.numeric(train[ycol])
    
  }
}

################################
# CART
################################

wrapperCART <- function(type, y, train, test = NULL, ordered.vars = NULL, count = F,
                        cp = .001, xval = 10L, prior = NULL, loss = NULL, split = "gini", ...) {
  
  ycol = grep(y[[1]], names(train))
  
  if(type == "surv" && "time" %in% y) {
    tcol <- grep(y[["time"]], names(train)) 
  }
  
  # Missing data warnings
  # if(is.na(train[ycol]) || is.na(train[y[["time"]]]) || any(!complete.cases(train[, -ycol, drop = F]))) {
  #   
  #   missing_y = which(is.na(train[ycol])) 
  #   missing_time = which(is.na(train[y[["time"]]]))
  #   missing_all = which(!complete.cases(train[, -ycol, drop = F]))
  #   
  #   
  #   warning(paste("Removed", length(missing_y), "cases (rows:", missing_y, ") with missing y values and",  
  #                 length(missing_all), "cases (rows:", missing_all, ") with no predictor data.", sep = " ")) 
  # } # Improve wording. And double check how missing surv data is treated.
  
  # Ensuring any factors with ordered levels are recognised by rpart as ordered factors
  # to speed up computation - see last sentence of pg. 29 of the 'long intro'. 
  train[ordered.vars] <- lapply(train[ordered.vars],
                               function(x) if(!is.ordered(x)) factor(x, ordered = T) else x) 
  
  fo <- sprintf("%s ~ %s", y[[1]],
                paste(colnames(train[!names(train) %in% y[[1]]]), collapse = "+"))
  fo <- as.formula(fo)
  
  if(type == "surv" && "time" %in% y) {
    update(fo, survival::Surv(y[["time"]], y[[1]]) ~ . - y[["time"]])
  }
  
  method = switch(type,
                  "classif" = "class",
                  "regr" = if(count) "poisson" else "anova",
                  "surv" = if("time" %in% y) "poisson" else "exp")
  
  # Not sure if there is a better way to do this
  allArgs <- c(mget(names(formals()), sys.frame(sys.nframe())), list(...))
  ctl <- do.call("rpart.control", allArgs[formalArgs(rpart.control)])
  
  # rpart will ignore "param" if method = anova but I would prefer setting it to null if regr is chosen.
  # Need to add the coeff variation for exp and poisson methods
  param = if(method == "class") allArgs[c("prior", "loss", "split")] else NULL
  
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
  
  
  # Plot of pruned tree
  
  
  # Variable importance plot
  imp = fit$variable.importance
  
  imp.plot = ggplot2::ggplot(data.frame(variable = names(imp), importance = imp), 
                             aes(x = reorder(variable, importance), y = importance)) +
    geom_bar(stat = "identity") + 
    labs(x = "Variable", y = "Importance") +
    coord_flip()
  
  imp.plot <- plotly::ggplotly(imp.plot)
  
  # ICE plots
  # ice.plot <- ICEbox::ice() # Need to decide layout
  
  output = list(fit.pruned, error, tree.plot, imp.plot, ice.plot)
  
  return(output)
}

library(rpart)
data(car90)
b = wrapperCART(train = car90, y = "Price", type = "regr")

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
  
  # Variable importance plots
  
  # p-values (need to look into holdoutRF as well)
  pval = importance_pvalues(fit, method = "altmann")
  
  # ICE plots
  
  

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
  
  
    
    






