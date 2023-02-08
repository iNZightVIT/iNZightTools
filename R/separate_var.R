#' Separate columns
#'
#' @param data dataset
#' @param var name of variable to be separated
#' @param by a string as delimiter between values (separate by delimiter) or an
#'        integer as number of characters to split by (separate by position)
#' @param left name for the separated left column (for \code{into = "cols"})
#' @param right name for the separated right column (for \code{into = "cols"})
#' @param into whether to split into new rows or columns
#'
#' @return Separated dataset
#' @rdname separate_var
#' @export
#' @author Stephen Su
separate_var <- function(data, var, by, left, right,
                         into = c("cols", "rows")) {
    expr <- rlang::enexpr(data)
    into <- ifelse(rlang::arg_match(into) == "cols", "wider", "longer")
    if (is_survey(data) && into == "longer") {
        rlang::abort("Cannot create new rows in surveys.")
    }
    if (!inherits(by, c("integer", "numeric", "character")) || length(by) > 1) {
        rlang::abort("Expecting `by` to be a string or a positive integer.")
    } else if (is.numeric(by)) {
        if (by != as.integer(by)) {
            rlang::warn(sprintf("Rounding to `by = %s`.", as.integer(by)))
        }
        by <- as.integer(by)
        var_nchar <- max(nchar(as.character(data[[var]])))
        if (by < 1 || by >= var_nchar) {
            rlang::abort(paste(
                "The value of `by` must be positive and strictly less than the",
                "length of the longest string in the variable to be separated."
            ))
        }
    }
    method <- ifelse(is.character(by), "delim", "position")
    sep_fn <- sprintf("tidyr::separate_%s_%s", into, method)
    sep_args <- list(rlang::sym(var))
    if (grepl("wider_position", sep_fn)) {
        by <- rlang::parse_expr(sprintf(
            "c(%s = %s, %s = %s)",
            left, by, right, var_nchar - by
        ))
    }
    sep_args <- c(sep_args, by)
    if (grepl("wider_delim", sep_fn)) {
        sep_args <- sep_args |>
            c(names = rlang::expr(c(!!left, !!right)), too_many = "merge")
    }
    if (grepl("wider", sep_fn)) {
        sep_args <- c(sep_args, too_few = "align_start")
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
