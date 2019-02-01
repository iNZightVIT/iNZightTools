#' Is numeric check
#' 
#' This function checks if a variable is numeric,
#' or could be considered one.
#' For example, dates and times can be treated as numeric,
#' so return \code{TRUE}.
#' 
#' @param x the variable to check
#' @return logical, \code{TRUE} if the variable is numeric
#' @author Tom Elliott
#' @export
is_numeric <- function(x) {
    vartype(x) %in% c('numeric', 'datetime')
}

#' Is factor check
#' 
#' This function checks if a variable a factor.
#' 
#' @param x the variable to check
#' @return logical, \code{TRUE} if the variable is numeric
#' @author Tom Elliott
#' @export
is_factor <- function(x) {
    vartype(x) == 'factor'
}

#' Get variable type name
#' 
#' @param x vector to be examined
#' @return character vector of the variable's type
#' @author Tom Elliott
#' @export
vartype <- function(x) {
    if (inherits(x, 'POSIXct') || 
        inherits(x, 'Date') || 
        inherits(x, 'time')) return('datetime')

    if (is.numeric(x)) 'numeric' else 'factor'
}
