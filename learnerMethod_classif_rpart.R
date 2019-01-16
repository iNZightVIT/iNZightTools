learnerMethod.classif.rpart <- function() {
  
  lrn = list(cl = "classif.rpart",
             
             library = "rpart",
             
             name = "CART Decision Tree",
             
             short.name = "rpart",
             
             par.set = createParamSet(
               createParam(id = "minsplit", type = "integer", default = 20L, lower = 1L),
               createParam(id = "minbucket", type = "integer", lower = 1L),
               createParam(id = "cp", type = "numeric", default = 0.001, lower = 0, upper = 1),
               createParam(id = "maxcompete", type = "integer", default = 4L, lower = 0L),
               createParam(id = "maxsurrogate", type = "integer", default = 5L, lower = 0L),
               createParam(id = "usesurrogate", type = "discrete", default = 2L, values = 0:2),
               createParam(id = "surrogatestyle", type = "discrete", default = 0L, values = 0:1),
               createParam(id = "maxdepth", type = "integer", default = 30L, lower = 1L, upper = 30L), # mlr uses 30 as upper limit, rpart.control ____
               createParam(id = "xval", type = "integer", default = 10L, lower = 0L, tunable = FALSE),
               createParam(id = "parms", type = "untyped")
             ),
             
             callees = c("rpart", "rpart.control"))

  
    class(lrn) = "classif.rpart"
    
    return(lrn)
  
}


