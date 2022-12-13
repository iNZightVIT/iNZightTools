#' Filter data by levels of numeric variables
#'
#' Filter a dataframe by some boolean condition of one numeric variable
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param data a dataframe or survey design object to filter
#' @param var character of the column in `data` to filter by
#' @param num a number for which the \code{op} applies to
#' @param op  a logical operator of "<=", "<", ">=", ">", "==" or "!="
#'        for the boolean condition
#' @return filtered dataframe with tidyverse code attached
#' @seealso \code{\link{code}}
#' @rdname filter_dbl
#' @examples
#' filtered <- filter_dbl(iris, var = "Sepal.Length", op = "<=", num = 5)
#' code(filtered)
#' head(filtered)
#'
#' library(survey)
#' data(api)
#' svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)
#' svy_filtered <- filter_dbl(svy, var = "api00", op = "<", num = 700)
#' code(svy_filtered)
#'
#' @author Owen Jin, Tom Elliott, Stephen Su
#' @export
#' @md
filter_dbl <- function(data, var, num,
                       op = c("<=", "<", ">=", ">", "==", "!=")) {
    op <- arg_match(op)
    exp <- expr_deparse(enexpr(data)) |>
        paste0(if_else(
            is_survey(data) && !inherits(data, "tbl_svy"),
            " |> srvyr::as_survey()", ""
        )) |>
        paste0(" |> ", sprintf(
            "%s::filter(%s %s %s)",
            if_else(is_survey(data), "srvyr", "dplyr"),
            var, op, num
        )) |>
        parse_expr()

    eval_tidy(exp) |> structure(code = exp)
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
#' @rdname filter_chr
#' @examples
#' filtered <- filter_chr(iris,
#'     var = "Species",
#'     levels = c("versicolor", "virginica")
#' )
#' code(filtered)
#' head(filtered)
#'
#' @author Owen Jin, Stephen Su
#' @export
filter_chr <- function(data, var, levels) {
    exp <- expr_deparse(enexpr(data)) |>
        paste0(if_else(
            is_survey(data) && !inherits(data, "tbl_svy"),
            " |> srvyr::as_survey()", ""
        )) |>
        paste0(" |> ", sprintf(
            "%s::filter(%s %s %s)",
            if_else(is_survey(data), "srvyr", "dplyr"),
            var, "%in%", expr_deparse(enexpr(levels))
        )) |>
        parse_expr()

    eval_tidy(exp) |> structure(code = exp)
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
#' @rdname rows_remove
#' @examples
#' filtered <- rows_remove(iris, rows = c(1, 4, 5))
#' code(filtered)
#' head(filtered)
#'
#' @author Owen Jin, Stephen Su
#' @export
rows_remove <- function(data, rows) {
    exp <- expr_deparse(enexpr(data))
    if (is_survey(data)) {
        exp <- sprintf("%s[-(%s)]", exp, expr_deparse(enexpr(rows)))
    } else {
        exp <- sprintf("dplyr::slice(%s, -(%s))", exp, expr_deparse(enexpr(rows)))
    }
    exp <- parse_expr(exp)

    eval_tidy(exp) |> structure(code = exp)
}


#' Random sampling without replacement
#'
#' Take a specified number of groups of observations with fixed group size
#' by sampling without replacement
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param data a dataframe to sample from
#' @param n the number of groups to generate
#' @param sample_size  the size of each group specified in \code{n}
#' @return a dataframe containing the random samples with
#'         tidyverse code attached
#' @seealso \code{\link{code}}
#' @rdname random_sample
#' @examples
#' filtered <- random_sample(iris, n = 5, sample_size = 3)
#' code(filtered)
#' head(filtered)
#'
#' @author Owen Jin, Stephen Su
#' @export
random_sample <- function(data, n, sample_size) {
    exp <- expr_deparse(enexpr(data))
    if (is_survey(data)) {
        abort("Survey data cannot be sampled at this stage.")
    }
    exp <- exp |>
        paste0(" |> ", sprintf(
            "dplyr::slice_sample(n = %s * %s)", n, sample_size
        )) |>
        paste0(" |> ", sprintf(
            "dplyr::mutate(.group = factor(rep(seq_len(%s), each = %s)))",
            n, sample_size
        )) |>
        parse_expr()

    eval_tidy(exp) |> structure(code = exp)
}
