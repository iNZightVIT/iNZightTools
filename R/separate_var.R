#' Separate columns
#'
#' @param data dataset
#' @param var name of variable to be separated
#' @param left name for the separated left column (for \code{into = "cols"})
#' @param right name for the separated right column (for \code{into = "cols"})
#' @param by a string as delimiter between values (\code{delim} method) or an
#'        integer as number of characters to split by (\code{position} method)
#' @param into whether to split into new rows or columns
#' @param method method of separation
#'
#' @return Separated dataset
#' @rdname separate_var
#' @export
#' @author Stephen Su
separate_var <- function(data, var, left, right, by,
                         into = c("cols", "rows"),
                         method = c("delim", "position")) {
    expr <- rlang::enexpr(data)
    into <- ifelse(rlang::arg_match(into) == "cols", "wider", "longer")
    method <- rlang::arg_match(method)
    if (is_survey(data) && into == "longer") {
        rlang::abort("Cannot create new rows in surveys.")
    }
    sep_fn <- sprintf("tidyr::separate_%s_%s", into, method)
    sep_args <- list(rlang::sym(var))
    if (grepl("wider_position", sep_fn)) {
        by <- rlang::parse_expr(sprintf(
            "c(%s = %s, %s = %s)", left, by, right,
            nchar(as.character(data[[var]][1])) - by
        ))
    }
    sep_args <- c(sep_args, by)
    if (grepl("wider_delim", sep_fn)) {
        sep_args <- c(sep_args, names = rlang::expr(c(!!left, !!right)))
    }
    sep_expr <- rlang::expr((!!rlang::parse_expr(sep_fn))(!!!sep_args))
    if (is_survey(data)) {
        expr <- rlang::expr(!!expr %>% (\(x) {
            x$variables <- x$variables %>% !!sep_expr
            x
        })())
    } else {
        expr <- rlang::expr(!!expr %>% !!sep_expr)
    }
    eval_code(expr)
}
