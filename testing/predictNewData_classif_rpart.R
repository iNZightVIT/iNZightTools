predictNewData.class.rpart <- function(model, newdata) {
  
  type = switch(model$learner$predict.type, 
                response = "class", 
                probability = "prob")
  
  
  pred = predict(model$model.fit, newdata = newdata, type = type)
  
  return(pred)
  
}