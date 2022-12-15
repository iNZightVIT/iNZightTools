#' Filter data by levels of numeric variables
#'
#' Filter a dataframe by some boolean condition of one numeric variable
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param data a dataframe or survey design object to filter
#' @param var character of the column in `data` to filter by
#' @param op  a logical operator of "<=", "<", ">=", ">", "==" or "!="
#'        for the boolean condition
#' @param num a number for which the \code{op} applies to
#' @return filtered dataframe with tidyverse code attached
#' @seealso \code{\link{code}}
#' @rdname filter_num
#' @examples
#' filtered <- filter_num(iris, var = "Sepal.Length", op = "<=", num = 5)
#' cat(code(filtered))
#' head(filtered)
#'
#' library(survey)
#' data(api)
#' svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)
#' svy_filtered <- filter_num(svy, var = "api00", op = "<", num = 700)
#' cat(code(svy_filtered))
#'
#' @author Owen Jin, Tom Elliott, Stephen Su
#' @export
#' @md
filter_num <- function(data, var,
                       op = c("<=", "<", ">=", ">", "==", "!="),
                       num) {
    op <- rlang::arg_match(op)
    expr <- rlang::enexpr(data)
    if (is_survey(data) && !inherits(data, "tbl_svy")) {
        expr <- rlang::expr(!!expr %>% srvyr::as_survey())
    }
    filter_lib <- rlang::sym(ifelse(is_survey(data), "srvyr", "dplyr"))
    expr <- rlang::expr(!!expr %>%
        `::`(!!filter_lib, filter)((!!op)(!!rlang::sym(var), !!num)))
    eval_code(expr)
}


#' Filter data by levels of categorical variables
#'
#' Filter a dataframe by some levels of one categorical variable
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param data a dataframe or survey design object to filter
#' @param var character of the column in \code{data} to filter by
#' @param levels a character vector of levels in \code{var} to filter by
#'
#' @return filtered dataframe with tidyverse code attached
#' @seealso \code{\link{code}}
#' @rdname filter_cat
#' @examples
#' filtered <- filter_cat(iris,
#'     var = "Species",
#'     levels = c("versicolor", "virginica")
#' )
#' cat(code(filtered))
#' head(filtered)
#'
#' @author Owen Jin, Stephen Su
#' @export
filter_cat <- function(data, var, levels) {
    expr <- rlang::enexpr(data)
    lvls <- rlang::enexpr(levels)
    if (is_survey(data) && !inherits(data, "tbl_svy")) {
        expr <- rlang::expr(!!expr %>% srvyr::as_survey())
    }
    filter_lib <- rlang::sym(ifelse(is_survey(data), "srvyr", "dplyr"))
    op <- ifelse(length(levels) > 1, "%in%", "==")
    expr <- rlang::expr(!!expr %>%
        `::`(!!filter_lib, filter)((!!op)(!!rlang::sym(var), !!lvls)))
    eval_code(expr)
}


#' Remove rows from data by row numbers
#'
#' Filter a dataframe by slicing off specified rows
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param data a dataframe or a survey design object to filter
#' @param rows  a numeric vector of row numbers to slice off
#' @return filtered dataframe with tidyverse code attached
#' @seealso \code{\link{code}}
#' @rdname remove_rows
#' @examples
#' data <- remove_rows(iris, rows = c(1, 4, 5))
#' cat(code(data))
#' head(data)
#'
#' @author Owen Jin, Stephen Su
#' @export
remove_rows <- function(data, rows) {
    expr <- rlang::enexpr(data)
    if (is_survey(data)) {
        expr <- rlang::expr(`[`(!!expr, -((!!rlang::enexpr(rows)))))
    } else {
        expr <- rlang::expr(dplyr::slice(!!expr, -((!!rlang::enexpr(rows)))))
    }
    eval_code(expr)
}
