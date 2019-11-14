#' Sort data by variables
#'
#' Sorts a dataframe by one or more variables, and returns the result
#' along with tidyverse code used to generate it.
#'
#' @param .data a dataframe to sort
#' @param vars  a character vector of variable names to sort by
#' @param asc   logical, same length as \code{vars}.
#'              If \code{TRUE} (default), sorted in ascending order,
#'              otherwise descending.
#' @return data.frame with tidyverse code attached
#' @seealso \code{\link{code}}
#' @examples
#' sorted <- sortVars(iris, vars = c("Sepal.Width", "Sepal.Length"),
#'     asc = c(TRUE, FALSE))
#' cat(code(sorted))
#' head(sorted)
#'
#' @author Owen Jin
#' @export
sortVars <- function(.data, vars, asc = rep(TRUE, length(vars))) {
    mc <- match.call()
    dataname <- mc$.data

    # paste together variables names adding desc() where needed
    eval_str <- ifelse(asc, vars, str_c("desc(", vars, ")", sep = "")) %>%
        str_c(collapse = ", ")

    exp <- ~.DATA %>%
        dplyr::arrange(.EVAL)
    exp <- replaceVars(exp,
        .DATA = dataname,
        .EVAL = eval_str
    )

    interpolate(exp)
}
