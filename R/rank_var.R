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

    formulae <- sapply(vars,
        function(var) {
            fmla <- ".VNAME.rank = dplyr::min_rank(.VAR)"
            fmla <- gsub(".VAR",
                ifelse(is_survey(.data), ".VNAME", ".DATA$.VNAME"),
                fmla
            )
            if (!is_survey(.data)) {
                fmla <- sprintf(
                    "tibble::add_column(%s, .after = \".VNAME\")",
                    fmla
                )
            }
            fmla <- gsub(".VNAME", var, fmla)
            if (is_survey(.data)) return(fmla)
            eval(parse(text = sprintf("~%s", fmla)))
        }
    )

    if (is_survey(.data)) {
        exp <- ~.DATA %>% update(.FMLA)
        formula <- paste(formulae, collapse = ", ")
        exp <- replaceVars(exp, .FMLA = formula)
    } else {
        exp <- pasteFormulae(c(list(~.DATA), as.list(formulae)))
    }

    exp <- replaceVars(exp, .DATA = dataname)
    interpolate(exp)
}
