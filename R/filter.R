filter_expr <- function(expr, var_expr, data) {
    if (is_survey(data)) {
        expr <- coerce_tbl_svy(expr, data)
        expr <- rlang::expr(!!expr %>% srvyr::filter(!!var_expr))
    } else {
        expr <- rlang::expr(!!expr %>% dplyr::filter(!!var_expr))
    }
}


#' Filter data by levels of numeric variables
#'
#' This function filters a dataframe or survey design object by applying a
#' specified boolean condition to one of its numeric variables. The resulting
#' filtered dataframe is returned, along with the tidyverse code used to
#' generate it.
#'
#' @param data A dataframe or survey design object to be filtered.
#' @param var The name of the column in `data` to be filtered by.
#' @param op  A logical operator to apply for the filtering condition.
#'        Valid options are: "<=", "<", ">=", ">", "==", or "!=".
#' @param num The numeric value for which the specified `op` is applied.
#' @return A filtered dataframe with the tidyverse code attached.
#' @seealso \code{\link{code}}
#' @examples
#' filtered <- filter_num(iris, var = "Sepal.Length", op = "<=", num = 5)
#' cat(code(filtered))
#' head(filtered)
#'
#' library(survey)
#' data(api)
#' svy <- svydesign(~ dnum + snum,
#'     weights = ~pw, fpc = ~ fpc1 + fpc2,
#'     data = apiclus2
#' )
#' svy_filtered <- filter_num(svy, var = "api00", op = "<", num = 700)
#' cat(code(svy_filtered))
#'
#' @author Owen Jin, Tom Elliott, Zhaoming Su
#' @export
#' @md
filter_num <- function(data, var,
                       op = c("<=", "<", ">=", ">", "==", "!="),
                       num) {
    op <- rlang::arg_match(op)
    expr <- rlang::enexpr(data)
    ## Defuse {`op`(var, num)} into the form of {var `op` num}
    var_expr <- rlang::expr((!!op)(!!rlang::sym(var), !!num))
    expr <- filter_expr(expr, var_expr, data)
    eval_code(expr)
}


#' Filter data by levels of categorical variables
#'
#' This function filters a dataframe or survey design object by keeping only
#' the rows where a specified categorical variable matches one of the given
#' levels. The resulting filtered dataframe is returned, along with the
#' tidyverse code used to generate it.
#'
#' @param data A dataframe or survey design object to be filtered.
#' @param var The name of the column in \code{data} to be filtered by.
#' @param levels A character vector of levels in \code{var} to keep.
#'
#' @return A filtered dataframe with the tidyverse code attached.
#' @seealso \code{\link{code}}
#' @examples
#' filtered <- filter_cat(iris,
#'     var = "Species",
#'     levels = c("versicolor", "virginica")
#' )
#' cat(code(filtered))
#' head(filtered)
#'
#' @author Owen Jin, Zhaoming Su
#' @export
#' @md
filter_cat <- function(data, var, levels) {
    expr <- rlang::enexpr(data)
    lvls <- rlang::enexpr(levels)
    op <- ifelse(length(levels) > 1, "%in%", "==")
    var_expr <- rlang::expr((!!op)(!!rlang::sym(var), !!lvls)) ## Defuse `op`
    expr <- filter_expr(expr, var_expr, data)
    eval_code(expr)
}

#' Remove rows from data by row numbers
#'
#' This function filters a dataframe or a survey design object by removing
#' specified rows based on the provided row numbers. The resulting filtered
#' dataframe is returned, along with the tidyverse code used to generate it.
#'
#' @param data A dataframe or a survey design object to be filtered.
#' @param rows A numeric vector of row numbers to be sliced off.
#' @return A filtered dataframe with the tidyverse code attached.
#'
#' @seealso \code{\link{code}}
#' @examples
#' data <- remove_rows(iris, rows = c(1, 4, 5))
#' cat(code(data))
#' head(data)
#'
#' @author Owen Jin, Zhaoming Su
#' @export
#' @md
remove_rows <- function(data, rows) {
    expr <- rlang::enexpr(data)
    if (is_survey(data)) {
        expr <- rlang::expr(`[`(!!expr, -((!!rlang::enexpr(rows)))))
    } else {
        expr <- rlang::expr(dplyr::slice(!!expr, -((!!rlang::enexpr(rows)))))
    }
    eval_code(expr)
}
