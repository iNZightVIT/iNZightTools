# EXAMPLES

# classif.rpart
data("iris")

task.iris = createTask(data = iris, target = "Species")

lrnr = createLearner(task = task.iris, learner.name = "rpart", param.user = list(cp = 0.0001))

mod = trainRLearner(task = task.iris, learner = learner.rpart)

mod2 = rpart("Species ~ .", data = iris, 
             minsplit = 20, minbucket = NULL, cp = 1e-04, maxcompete = 4, 
             maxsurrogate = 5, usesurrogate = 2, surrogatestyle = 0, maxdepth = 30, xval = 10) 

# Why does mod2 work but mod doesnt??

