#' Collapse data by values of a categorical variable
#' 
#' Collapse several values in a categorical variable into one level
#' 
#' @param .data a dataframe to collapse
#' 
#' @param var  a character of the name of the catgorical variable to collapse
#'
#' @param levels  a character vector of the levels to be collapsed
#' 
#' @return the original dataframe containing a new column of the collapsed variable with tidyverse code attached
#' @seealso \code{\link{code}} 
#' 
#' @examples
#' collapsed <- collapseLevels(iris, var = "Species", levels = c("versicolor", "virginica"))
#' code(collapsed)
#' head(collapsed)
#' 
#' @author Owen Jin
#' @export
#' 
#' 
#get.collapsed.column = function(column,to.collapse){
collapseLevels <- function(.data, var, levels){
  mc <- match.call()
  dataname <- mc$.data
  
  exp <- ~.DATA %>%
    tibble::add_column(.VARNAME.coll = forcats::fct_collapse(.DATA$.VARNAME, .COLLAPSENAME = .LEVELS), .after = ".VARNAME")
  exp <- replaceVars(exp, .VARNAME = var, .COLLAPSENAME = str_c(levels, collapse = "_"), .LEVELS = levels, .DATA = dataname)
  
  interpolate(exp)
}