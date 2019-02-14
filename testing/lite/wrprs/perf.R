bmrParallel <- function(task, learners, resampling, measures, keep.pred = T, keep.models = T) {
  
  # Using parallelisation to speed up computation
  parallelStart()
  
  results = parallelMap(doResampling, learners, more.args = as.list(match.call(expand.dots = T)))
                                         
  
  parallelStop()
  
  bmr = list(results = results, 
             learners = learners, 
             measures = measures)
  
  class(bmr) <- "bmrResult"
  
}


################################

doResampling <- function(learner, task, rin, measures, keep.pred = T, keep.models = T) {
  
  iters <- seq_len(rin$desc$iters)
  
  parallelMap(doIteration, iters, more.args = as.list(match.call(expand.dots = T)))
  
  
}

##############################

doIteration <- function(iter, learner, task, rin, measures) {
  
  data <- task$env$data
  
  train.idx = rin$train.inds[[iter]]
  test.idx = rin$test.inds[[iter]]
  
  mod = trainRLearner(task, learner, subset = train.idx)
  
  pred = predictNewData(mod, data[test.idx, ]) #should I add a subset argument instead? Also, what's the point in predicting both the test and training sets?
  
  perf = lapply(measures, measurePerf)
    
  res = list(model = mod,
             pred.test = pred, 
             perf.test = perf)
  
  return(res)
  
  
}

###########################

measurePerf <- function(task, model, pred, measure) {
  
  


}

##########################

perfMethod <- function() {
  
  UseMethod("perfMethod")
  
}
  
##########################


perfMethod.mmce <- function(truth, pred) {
  
  
  
  
  
}

task <- createTask(iris, "Species", setdiff(names(iris), "Species"))
learner <- addLearner(task, "rpart", "response")
mod <- trainRLearner(task, learner)
pred <- predictRLearner(mod, task)


library(mlr)
lrns = list(
  makeLearner("classif.rpart", id = "rpart"),
  makeLearner("classif.C50", id = "C50"),
  makeLearner("classif.randomForest", id = "randomForest")
)

tasks = list(iris.task)

rdesc = makeResampleDesc("CV", iters = 10)
rdesc2 = makeResampleDesc("CV", rep = 2, folds = 5)

rin = makeResampleInstance(rdesc, iris.task)
rin2 = makeResampleInstance(rdesc2, iris.task)

ensureBenchmarkLearners = function(learners) {
  learner.ids = vcapply(learners, mlr::getLearnerId)
  if (anyDuplicated(learner.ids))
    stop("Learners need unique ids!")
  setNames(learners, learner.ids)
}

lrns = ensureBenchmarkLearners(lrns)