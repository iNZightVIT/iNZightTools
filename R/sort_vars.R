#' Sort data by variables
#'
#' Sorts a dataframe by one or more variables, and returns the result
#' along with tidyverse code used to generate it.
#'
#' @param data a dataframe to sort
#' @param vars a character vector of variable names to sort by
#' @param asc  logical, length of 1 or same length as \code{vars}.
#'        If \code{TRUE} (default), then sorted in ascending order,
#'        otherwise descending.
#' @return data with tidyverse code attached
#' @rdname sort_vars
#' @seealso \code{\link{code}}
#' @examples
#' sorted <- sort_vars(iris,
#'     vars = c("Sepal.Width", "Sepal.Length"),
#'     asc = c(TRUE, FALSE)
#' )
#' cat(code(sorted))
#' head(sorted)
#'
#' @author Owen Jin, Zhaoming Su
#' @export
sort_vars <- function(data, vars, asc = rep(TRUE, length(vars))) {
    expr <- rlang::enexpr(data)
    if (length(asc) != length(vars) && length(asc) != 1L) {
        rlang::abort("`asc` should have length of 1 or equal length as `vars`.")
    }
    sort_expr <- ifelse(asc, vars, sprintf("desc(%s)", vars)) |>
        rlang::parse_exprs()
    if (is_survey(data)) {
        expr <- rlang::expr(!!expr %>% (\(x) {
            x$variables <- dplyr::arrange(x$variables, !!!sort_expr)
            x
        })())
    } else {
        expr <- rlang::expr(!!expr %>% dplyr::arrange(!!!sort_expr))
    }
    eval_code(expr)
}
