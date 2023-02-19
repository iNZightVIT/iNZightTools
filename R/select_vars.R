#' Select variables from a dataset
#'
#' Select a (reordered) subset of variables from a subset.
#'
#' @param data the dataset
#' @param keep vector of variable names to keep
#' @return a data frame with tidyverse code attribute
#' @rdname select_vars
#' @author Tom Elliott, Zhaoming Su
#' @examples
#' select_vars(iris, c("Sepal.Length", "Species", "Sepal.Width"))
#' @export
select_vars <- function(data, keep) {
    expr <- rlang::enexpr(data)
    if (is_survey(data)) {
        expr <- coerce_tbl_svy(expr, data)
        expr <- rlang::expr(!!expr %>% srvyr::select(!!!rlang::syms(keep)))
    } else {
        expr <- rlang::expr(!!expr %>% dplyr::select(!!!rlang::syms(keep)))
    }
    eval_code(expr)
}
