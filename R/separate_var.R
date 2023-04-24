#' Separate columns
#'
#' @param data dataset
#' @param var name of variable to be separated
#' @param by a string as delimiter between values (separate by delimiter) or
#'        integer(s) as number of characters to split by (separate by position),
#'        the length of \code{by} should be \code{1} unless \code{by} is integer
#'        and \code{into = "cols"}; if \code{by} is a non-integer numeric vector
#'        its values will be rounded down to the nearest integer
#' @param names for \code{into = "cols"}, a character vector of output column
#'        names; use \code{NA} if there are components that you don't want to
#'        appear in the output; the number of non-\code{NA} elements determines
#'        the number of new columns in the result
#' @param into whether to split into new rows or columns
#'
#' @return Separated dataset
#' @rdname separate_var
#' @export
#' @author Zhaoming Su
separate_var <- function(data, var, by, names, into = c("cols", "rows")) {
    expr <- rlang::enexpr(data)
    into <- ifelse(rlang::arg_match(into) == "cols", "wider", "longer")
    if (is_survey(data) && into == "longer") {
        rlang::abort("Cannot create new rows in surveys.")
    }
    if (!inherits(by, c("integer", "numeric", "character"))) {
        rlang::abort("Expecting `by` to be a string or positive integers.")
    }
    if (is.numeric(by)) {
        if (any(by != as.integer(by))) {
            rlang::warn("Rounding `by` down to nearest integer(s).")
        }
        by <- floor(by)
        vnc <- max(nchar(as.character(data[[var]])))
        if (any(by < 1) || any(by >= vnc)) {
            rlang::abort(paste(
                "The value of `by` must be positive and strictly less than the",
                "length of the longest string in the variable to be separated."
            ))
        }
    }
    method <- ifelse(is.character(by), "delim", "position")
    sep_fn <- sprintf("tidyr::separate_%s_%s", into, method)
    sep_arg <- list(rlang::sym(var))
    if (grepl("wider_position", sep_fn)) {
        if (length(by) != length(names) - 1) {
            rlang::abort("Length of `by` must be shorter than `names` by 1.")
        }
        names <- tidyr::replace_na(names, "")
        by <- rlang::expr(c(!!!rlang::set_names(diff(c(0, by, vnc)), names)))
    } else if (length(by) > 1) {
        rlang::abort("Length of `by` must be 1.")
    }
    sep_arg <- c(sep_arg, by)
    if (grepl("wider_delim", sep_fn)) {
        sep_arg <- c(sep_arg, names = rlang::enexpr(names), too_many = "merge")
    }
    if (grepl("wider", sep_fn)) {
        sep_arg <- c(sep_arg, too_few = "align_start", names_repair = "unique")
    }
    sep_expr <- rlang::expr((!!rlang::parse_expr(sep_fn))(!!!sep_arg))
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
