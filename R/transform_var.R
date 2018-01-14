#' Transform data of a numeric variable
#' 
#' Transform the values of a numeric variable by applying a mathematical function 
#' 
#' @param .data a dataframe with the variables to transform
#' 
#' @param var  a character of the numeric variable in \code{.data} to transform
#'
#' @param transformation  a name of a valid mathematical function that can be applied to numeric values, eg. "log", "exp", "sqrt". For squaring - use "square", for inversing - use "reciprocal"
#' 
#' @return the original dataframe containing a new column of the transformed variable with tidyverse code attached
#' @seealso \code{\link{code}} 
#' 
#' @examples
#' transformed <- transformVar(iris, var = "Petal.Length", transformation = "log"))
#' code(transformed)
#' head(transformed)
#' 
#' @author Owen Jin
#' @export
#' 
#' 

transformVar <- function(.data, var, transformation){
  
  # Below are custom functions for transform variables
  
  reciprocal <- function(x){
    return(1/x)
  }
  square <- function(x){
    return (x ^ 2)
  }
  
  mc <- match.call()
  dataname <- mc$.data
  
  funexp <- switch(transformation,
                   reciprocal = "1 / .DATA$.VARNAME",
                   square = ".DATA$.VARNAME^2",
                   ".FUNNAME(.DATA$.VARNAME)"
  )
  ## run replaceVars (or just use sprintf() above...) first
  
  
  exp <- ~.DATA %>%
    tibble::add_column(.VARNAME..FUNNAME = .FUNEXP, .after = ".VARNAME")
  #tibble::add_column(.VARNAME..F = .F(.DATA$.VARNAME), .after = ".VARNAME")
  exp <- replaceVars(exp, .FUNEXP = funexp, .DATA = dataname, .VARNAME = var, .FUNNAME = transformation)
  
  interpolate(exp)
}