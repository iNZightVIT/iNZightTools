#' Rank the data of a numeric variables
#'
#' Rank the values of a numeric variable in descending order,
#' and returns the result along with tidyverse code used to generate it.
#' Ties are broken as such: eg. values = 5, 6, 6, 7 ; rank = 1, 2, 2, 3
#'
#' @param .data a dataframe with the variables to rank
#' @param vars  a character vector of numeric variables in \code{.data}
#'        to rank
#'
#' @return the original dataframe containing new columns with the ranks of the
#'         variables in \code{var} with tidyverse code attached
#' @seealso \code{\link{code}}
#' @examples
#' ranked <- rankVars(iris, vars = c("Sepal.Length", "Petal.Length"))
#' cat(code(ranked))
#' head(ranked)
#'
#' @author Owen Jin
#' @export
rankVars <- function(.data, vars) {
    mc <- match.call()
    dataname <- mc$.data

    formulae <- list(~.DATA)

    for (i in seq_along(vars)) {
        formula <-
            ~tibble::add_column(
                .VARNAME.rank = dplyr::min_rank(.DATA$.VARNAME),
                .after = ".VARNAME"
            )
        formulae[[i + 1]] <- replaceVars(formula, .VARNAME = vars[i])
    }

    exp <- pasteFormulae(formulae)
    exp <- replaceVars(exp, .DATA = dataname)

    interpolate(exp)
}
