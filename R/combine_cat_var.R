#' Combine categorical variables into one
#'
#' Combine specified categorical variables by concatenating
#' their values into one character, and returns the result
#' along with tidyverse code used to generate it.
#'
#' @param .data a dataframe with the columns to be combined
#' @param vars  a character vector of the categorical variables to be combined
#' @param sep the separator to combine the values of the variables
#'        in \code{var} by. "." by default
#' @param name a name for the new variable
#' @return original dataframe containing a new column of the renamed
#'         categorical variable with tidyverse code attached
#'
#' @examples
#' combined <- combineCatVars(warpbreaks, vars = c("wool", "tension"), sep = "_")
#' cat(code(combined))
#' head(combined)
#'
#' @author Owen Jin
#' @export
#'
combineCatVars <- function(.data, vars, sep = ".",
                           name = paste(vars, collapse = sep)) {
    if (length(sep) > 1) {
        warning("only one separator allowed")
        sep <- sep[1]
    }
    sep <- substr(sep, 1, 1)
    valid_sep <- grepl("[.]|_|[a-zA-Z0-9]", sep)
    if (!valid_sep) {
        warning(paste(
            "You can only use dots (.), underscores (_),",
            "or alphanumeric characters for the separator.",
            "Using . instead."
        ))
        sep <- "."
    }
    mc <- match.call()
    dataname <- mc$.data

    # paste together the new variable made from the old variable names
    to_be_combined <- paste(vars, collapse = ", ")

    exp <- ~.DATA %>%
        dplyr::mutate(.NEWVAR = factor(paste(.VARS, sep = ".SEP")))
    exp <- replaceVars(exp,
        .DATA = dataname,
        .NEWVAR = name,
        .VARS = to_be_combined,
        .SEP = sep
    )

    interpolate(exp)
}
