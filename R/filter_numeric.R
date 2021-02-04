#' Filter data by levels of a numeric variables
#'
#' Filter a dataframe by some boolean condition of one numeric variable
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param .data a dataframe or survey design object to filter
#' @param var character of the column in `.data` to filter by
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
#' require(survey)
#' data(api)
#' svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)
#' (svy_filtered <- filterNumeric(svy, var = "api00", op = "<", num = 700))
#' cat(code(svy_filtered))
#'
#' @author Owen Jin, Tom Elliott
#' @export
#' @md
filterNumeric <- function(.data, var, op, num) {
    mc <- match.call()
    dataname <- mc$.data

    is_survey <- is_survey(.data)
    if (is_survey && !inherits(.data, "tbl_svy")) {
        .data <- srvyr::as_survey(.data)
        dataname <- glue::glue("{dataname} %>% srvyr::as_survey()")
    }

    exp <- ~.DATA %>% .FUN(.VARNAME.OP.NUM)
    exp <- replaceVars(exp,
        .VARNAME = var,
        .OP = op,
        .NUM = num,
        .DATA = dataname,
        .FUN = if (is_survey) "srvyr::filter" else "dplyr::filter"
    )
    interpolate(exp)
}
