#' Append rows to a dataset
#'
#' @param data The original dataset to which new rows will be appended.
#' @param new_data The dataset containing the new rows.
#' @param when_added Logical; indicates whether a \code{.when_added} column
#'        is required.
#' @return A dataset with new rows appended below the original \code{data}.
#' @author Yiwen He, Zhaoming Su
#' @md
#' @export
append_rows <- function(data, new_data, when_added = FALSE) {
    expr <- rlang::enexpr(data)
    new_expr <- rlang::enexpr(new_data)
    if (is_survey(data)) {
        rlang::abort("Cannot append rows to surveys.")
    }
    if (when_added) {
        new_expr <- rlang::expr((!!new_expr) |>
            dplyr::mutate(.when_added = Sys.time()))
    }
    expr <- rlang::expr(!!expr %>% dplyr::bind_rows(!!new_expr))
    eval_code(expr)
}
