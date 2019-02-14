learnerMethod.classif.rpart <- function() {
  
  lrn = list(cl = "classif.rpart",
             
             package = "rpart",
             
             name = "CART Decision Tree",
             
             short.name = "rpart",
             
             param = structure(createParamSet(createIntegerParam(id = "minsplit", default = 20L, lower = 1L),
                                              createIntegerParam(id = "minbucket", lower = 1L),
                                              createNumericParam(id = "cp", default = 0.001, lower = 0, upper = 1),
                                              createIntegerParam(id = "maxcompete", default = 4L, lower = 0L),
                                              createIntegerParam(id = "maxsurrogate", default = 5L, lower = 0L),
                                              createDiscreteParam(id = "usesurrogate", default = 2L, values = 0:2),
                                              createDiscreteParam(id = "surrogatestyle", default = 0L, values = 0:1),
                                              createIntegerParam(id = "maxdepth", default = 30L, lower = 1L, upper = 30L), # mlr uses 30 as upper limit, rpart.control ____
                                              createIntegerParam(id = "xval", default = 10L, lower = 0L, tunable = F)
                                              #createCharacterParam(id = "split", default = "gini", values = c("gini", "information")),
                                              #createVectorParam(id = "prior"),
                                              #createVectorParam(id = "loss")
                                              ),
                               class = "param.set"),
             
             param.user = NULL,
             
             callees = c("rpart", "rpart.control"))
  
  
  class(lrn) = "classif.rpart"
  
  return(lrn)
  
}

