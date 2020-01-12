#' Filter data by levels of a numeric variables
#'
#' Filter a dataframe by some boolean condition of one numeric variable
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param .data a dataframe to filter
#' @param var character of the column in \code{.data} to filter by
#' @param op  a logical operator of "<=", "<", ">=", ">", "==" or "!="
#'        for the boolean condition
#' @param num a number for which the \code{op} applies to
#' @return filtered dataframe with tidyverse code attached
#' @seealso \code{\link{code}}
#' @examples
#' filtered <- filterNumeric(iris, var = "Sepal.Length", op = "<=", num = 5)
#' cat(code(filtered))
#' head(filtered)
#'
#' @author Owen Jin
#' @export
filterNumeric <- function(.data, var, op, num) {
    mc <- match.call()
    dataname <- mc$.data

    exp <- ~.DATA %>% dplyr::filter(.VARNAME.OP.NUM)
    exp <- replaceVars(exp,
        .VARNAME = var,
        .OP = op,
        .NUM = num,
        .DATA = dataname
    )
    interpolate(exp)
}
