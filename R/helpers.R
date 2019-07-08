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
is_num <- function(x) {
    vartype(x) %in% c('num', 'dt')
}

#' Is factor check
#'
#' This function checks if a variable a factor.
#'
#' @param x the variable to check
#' @return logical, \code{TRUE} if the variable is a factor
#' @author Tom Elliott
#' @export
is_cat <- function(x) {
    vartype(x) == 'cat'
}

#' Is datetime check
#'
#' This function checks if a variable a date/time/datetime
#'
#' @param x the variable to check
#' @return logical, \code{TRUE} if the variable is a datetime
#' @author Tom Elliott
#' @export
is_dt <- function(x) {
    vartype(x) == 'dt'
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
        inherits(x, 'times') ||
        inherits(x, 'hms')) return('dt')

    if (is.numeric(x)) 'num' else 'cat'
}


#' Check if object is a survey object (either standard or replicate design)
#'
#' @param x object to be tested
#' @return logical
#' @author Tom Elliott
#' @export
is_survey <- function(x) {
    is_svydesign(x) || is_svyrep(x)
}

#' Check if object is a survey object (created by svydesign())
#'
#' @param x object to be tested
#' @return logical
#' @author Tom Elliott
#' @export
is_svydesign <- function(x) {
    inherits(x, "survey.design")
}

#' Check if object is a replicate survey object (created by svrepdesign())
#'
#' @param x object to be tested
#' @return logical
#' @author Tom Elliott
#' @export
is_svyrep <- function(x) {
    inherits(x, "svyrep.design")
}
