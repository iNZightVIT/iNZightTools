createParamSet <- function(...) {
  
  params = list(...)
  
  names(params) = sapply(params, "[[", "id")
  
  return(params)
  
}

######################

# Not sure if these functions are really needed  
# I left them in since it wouldn't be a good idea to hard-code the param values without any sanity checks 

createIntegerParam <- function(id, default, lower = -Inf, upper = Inf, tunable = T) {
  
  if (missing(default)) {
   
    default = NULL
    
  }
  
  # vals = c(default, lower, upper)
  # 
  # if(any(vals %% 1 != 0)) {
  #   
  #   stop("`default`, `lower` and `upper` must be integers.")
  # }
  
  if(upper < lower) {
    stop(sprintf("Check interval bounds for param '%s': `upper` cannot be less than `lower`!", id))
  }
  
  p = list(id = id, type = "integer", default = default, 
           lower = lower, upper = upper, tunable = tunable)
  
  class(p) <- c("param", "param.int")
  
  return(p)
}

#########################################

createNumericParam <- function(id, default, lower = -Inf, upper = Inf, tunable = T) {
  
  if (missing(default)) {
    
    default = NULL
    
  }
  
  if(upper < lower) {
    stop(sprintf("Check interval bounds for param '%s': `upper` cannot be less than `lower`!", id))
  }
  
  p = list(id = id, type = "numeric", default = default, 
           lower = lower, upper = upper, tunable = tunable)
  
  class(p) <- c("param", "param.num")
  
  return(p)
}

######################################

createDiscreteParam <- function(id, default, values, tunable = T) {
  
  if (missing(default)) {
    
    default = NULL
    
  }
  
  p = list(id = id, type = "discrete", default = default, 
           values = values, tunable = tunable)
  
  class(p) <- c("param", "param.disc")
  
  return(p)
}

###############################

createCharacterParam <- function(id, default, values, tunable = T) {
  
  if (missing(default)) {
    
    default = NULL
    
  }
  
  
  p = list(id = id, type = "character", default = default, 
           values = values, tunable = tunable)
  
  class(p) <- c("param", "param.untyped")
  
  return(p)
}