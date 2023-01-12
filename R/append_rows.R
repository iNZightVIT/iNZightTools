#' Append rows to the dataset
#'
#' @param data original dataset
#' @param new_data dataset to append below \code{data}
#' @param when_added whether a \code{.when_added} column is required
#' @return dataset with new rows appended
#' @rdname append_rows
#' @export
#' @author Yiwen He, Stephen Su
append_rows <- function(data, new_data, when_added = FALSE) {
    expr <- rlang::enexpr(data)
    new_expr <- rlang::enexpr(new_data)
    if (is_survey(data)) {
        rlang::abort("Cannot append rows to surveys.")
    }
    if (when_added) {
        new_expr <- rlang::expr((!!new_expr) |>
            dplyr::mutate(.when_added = lubridate::now()))
    }
    expr <- rlang::expr(!!expr %>% dplyr::bind_rows(!!new_expr))
    eval_code(expr)
}
