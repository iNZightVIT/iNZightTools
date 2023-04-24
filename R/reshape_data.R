#' Reshaping dataset from wide to long or from long to wide
#'
#' @param data a dataset to reshape
#' @param data_to whether the target dataset is \code{long} or \code{wide}
#' @param cols columns to gather together (for wide to long)
#' @param names_to name for new column containing old names (for wide to long)
#' @param values_to name for new column containing old values (for wide to long)
#' @param names_from column to spread out (for long to wide)
#' @param values_from values to be put in the spread columns (for long to wide)
#'
#' @return reshaped dataset
#' @rdname reshape_data
#' @author Zhaoming Su
#' @export
reshape_data <- function(data, data_to = c("long", "wide"),
                         cols, names_to = "name", values_to = "value",
                         names_from = "name", values_from = "value") {
    expr <- rlang::enexpr(data)
    data_to <- rlang::arg_match(data_to)
    if (data_to == "long") {
        if (length(cols) > 1L) {
            cols <- rlang::expr(c(!!!rlang::syms(cols)))
        } else {
            cols <- rlang::sym(cols)
        }
        expr <- rlang::expr(!!expr %>% tidyr::pivot_longer(
            !!cols,
            names_to = !!names_to,
            values_to = !!values_to,
            names_repair = "unique"
        ))
    } else {
        expr <- rlang::expr(!!expr %>% tidyr::pivot_wider(
            names_from = !!rlang::sym(names_from),
            values_from = !!rlang::sym(values_from),
            names_repair = "unique"
        ))
    }
    eval_code(expr)
}
