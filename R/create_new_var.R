#' Create new variables
#'
#' Create a new variable by using a valid R expression
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param .data a dataframe to which to add a new variable to
#' @param new_var  a character of the new variable name.
#' "new.variable" by default
#' @param R_exp a character of a valid R expression which can
#' generate a vector of values
#' @return original dataframe containing the new column
#'         created from \code{R_exp}
#' with tidyverse code attached
#' @seealso \code{\link{code}}
#' @examples
#' created <- createNewVar(iris, new_var = "Sepal.Length_less_Sepal.Width",
#'  "Sepal.Length - Sepal.Width")
#' cat(code(created))
#' head(created)
#'
#' @author Owen Jin
#' @export
createNewVar <- function(.data, new_var = "new.variable", R_exp) {
    mc <- match.call()
    dataname <- mc$.data

    if (is_survey(.data)) {
        exp <- ~.DATA %>% update(.VARNAME = .EVAL)
    } else {
        exp <- ~.DATA %>%
            dplyr::mutate(.VARNAME = .EVAL)
    }

    exp <- replaceVars(exp,
        .VARNAME = new_var,
        .EVAL = R_exp,
        .DATA = dataname
    )

    interpolate(exp)
}
