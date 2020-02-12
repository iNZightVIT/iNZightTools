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


#' Add suffix to string
#'
#' When creating new variables or modifying the data set, we often
#' add a suffix added to distinguish the new name from the original one.
#' However, if the same action is performed twice (for example, filtering a data set),
#' the suffix is duplicated (data.filtered.filtered). This function averts this
#' by adding the suffix if it doesn't exist, and otherwise appending
#' a counter (data.filtered2).
#'
#' @param name a character vector containing (original) names
#' @param suffix the suffix to add, a length-one character vector
#' @return character vector of names with suffix appended
#' @examples
#' add_suffix("data", "filtered")
#' add_suffix(c("data.filtered", "data.filtered.reshaped"), "filtered")
#' @export
add_suffix <- function(name, suffix) {
    if (length(suffix) > 1)
        warning("More than one suffix specified, using only the first.")
    suffix <- suffix[1]

    new_name <- sapply(name,
        function(x) {
            if (grepl(suffix, x, fixed = TRUE)) {
                # counter (numbers after suffix)
                expr <- paste0("\\.", suffix, "[0-9]*")
                rgx <- regexpr(expr, x)
                rgn <- gsub(paste0(".", suffix), "",
                    substr(x, rgx, rgx + attr(rgx, "match.length") - 1)
                )
                count <- max(1, as.integer(rgn), na.rm = TRUE) + 1
                gsub(expr, sprintf(".%s%d", suffix, count), x)
            } else {
                sprintf("%s.%s", x, suffix)
            }
        }
    )
    unname(new_name)
}
