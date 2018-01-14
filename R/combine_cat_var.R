#' Combine categorical variables into one
#' 
#' Combine specified categorical varaibles by concatenating 
#' their values into one character, and returns the result
#' along with tidyverse code used to generate it.
#'
#' @param .data a dataframe with the columns to be combined
#' 
#' @param var  a character vector of the categorical variables to be combined
#' 
#' @param sep the seperator to combine the values of the variables in \code{var} by.
#' "." by default
#' 
#' @return original dataframe containing a new column of the renamed categorical variable with tidyverse code attached
#' 
#' @seealso \code{\link{combine_levels}} ###### 
#' 
#' @examples
#' combined <- combineCatVars(warpbreaks, vars = c("wool", "tension"), sep = "_")
#' code(combined)
#' head(combined)
#' 
#' @author Owen Jin
#' @export
#' 


combineCatVars <- function(.data, vars, sep = "."){
  if (length(sep) > 1) {
    warning("only one separator allowed")
    sep <- sep[1]
  }
  if (!all(sapply(sep, function(x) 
    all(grepl("[.]|_|[a-zA-Z0-9]", strsplit(x, "")[[1]]))))) {
    warning("You can only use dots (.), underscores (_), or alphanumeric characters for the separator. Using . instead.")
    sep <- "."
  }
  mc <- match.call()
  dataname <- mc$.data
  
  # paste together the new variable made from the old variable names 
  new_var_name <- str_c(vars, collapse = sep)
  to_be_combined <- str_c(vars, collapse =", ")
  
  exp <- ~.DATA %>%
    dplyr::mutate(.NEWVAR = str_c(.VARS, sep = ".SEP"))
  exp <- replaceVars(exp, .DATA = dataname, .NEWVAR = new_var_name, .VARS = to_be_combined, .SEP = sep)
  
  interpolate(exp)
}