#' Transform data of a numeric variable
#' 
#' Transform the values of a numeric variable by applying a mathematical function 
#' 
#' @param .data a dataframe with the columns to standarize
#' 
#' @param vars  a character vector of the numeric variables in \code{.data} 
#' to standardize
#' 
#' @param names names for the created variables
#' 
#' @return the original dataframe containing new columns of the standardized variables with tidyverse code attached
#' @seealso \code{\link{code}} 
#' 
#' @examples
#' standardized <- standardizeVars(iris, var = c("Sepal.Width", "Petal.Width"))
#' code(standardized)
#' head(standardized)
#' 
#' @author Owen Jin
#' @export
#' 
#' 
#' 
standardizeVars <- function(.data, vars, names = paste(sep = ".", vars, "std")) {
  mc <- match.call()
  dataname <- mc$.data
  
  formulae <- list(~.DATA)
  
  for (i in 1:length(vars)){
    formula <- ~ tibble::add_column(.NAME = scale(.DATA$.VARNAME)[,1], .after = ".VARNAME")
    formulae[[i+1]] <- replaceVars(formula, .VARNAME = vars[i], .NAME = names[i])
  }
  
  exp <- pasteFormulae(formulae)
  exp <- replaceVars(exp, .DATA = dataname)
  
  interpolate(exp)
}