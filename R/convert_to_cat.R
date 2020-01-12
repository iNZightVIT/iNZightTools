#' Convert numeric variables to categorical
#'
#' Convert specified numeric variables into factors
#'
#' @param .data a dataframe with the categorical column to convert
#' @param vars  a character vector of numeric column names to convert
#' @param names a character vector of names for the created variable(s)
#' @return original dataframe containing a new column of the
#'         converted numeric variable with tidyverse code attached
#' @seealso \code{\link{code}}
#' @examples
#' converted <- convertToCat(iris, vars = c("Petal.Width"))
#' cat(code(converted))
#' head(converted)
#'
#' @author Owen Jin
#' @export
convertToCat <- function(.data, vars, names = paste(vars, "cat", sep = ".")) {
    mc <- match.call()
    dataname <- mc$.data

    formulae <- list(~.DATA)

    for (i in seq_along(vars)) {
        formula <- ~tibble::add_column(
            .NAME = factor(.DATA$.VARNAME),
            .after = ".VARNAME"
        )
        formula <- replaceVars(formula, .VARNAME = vars[i], .NAME = names[i])
        formulae[[i + 1]] <- formula
    }

    exp <- pasteFormulae(formulae)
    exp <- replaceVars(exp, .DATA = dataname)

    interpolate(exp)
}
