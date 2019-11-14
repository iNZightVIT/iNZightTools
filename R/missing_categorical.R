#' Convert missing values to categorical variables
#'
#' Turn <NA>'s into a "missing" character;
#' hence numeric variables will be converted to categorical variables
#' with any numeric values will be converted to "observed",
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param .data a dataframe with the columns to convert
#'        its missing values into categorical
#' @param vars  a character vector of the variables in \code{.data}
#'        for conversion of missing values to categorical
#' @param names a vector of names for the new variables
#' @return original dataframe containing new columns of the converted variables
#'        for the missing values
#'        with tidyverse code attached
#' @seealso \code{\link{code}}
#' @examples
#' missing <- missingToCat(iris, vars = c("Species", "Sepal.Length"))
#' cat(code(missing))
#' head(missing)
#'
#' @author Owen Jin
#' @export
missingToCat <- function(.data, vars, names = paste0(vars, "_miss")) {
    mc <- match.call()
    dataname <- mc$.data

    formulae <- list(~.DATA)

    numCols <- sapply(.data[, vars], is.numeric)

    for (i in seq_along(vars)) {
        if (numCols[i]) {
            ## it's a number
            formula <-
                ~tibble::add_column(
                    .NAME = factor(
                        ifelse(is.na(.DATA$.VARNAME),
                            "missing", "observed"
                        )
                    ),
                    .after = ".VARNAME"
                )
        }
        else {
            formula <-
                ~tibble::add_column(
                    .NAME = forcats::fct_explicit_na(
                        .DATA$.VARNAME,
                        na_level = "missing"
                    ),
                    .after = ".VARNAME"
                )
        }
        formula <- replaceVars(formula,
            .VARNAME = vars[i],
            .NAME = names[i]
        )
        formulae[[i + 1]] <- formula
    }

    exp <- pasteFormulae(formulae)
    exp <- replaceVars(exp, .DATA = dataname)

    interpolate(exp)
}
