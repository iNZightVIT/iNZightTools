#' Join data with another dataset
#'
#' @param data_l original data
#' @param data_r imported dataset
#' @param by a character vector of variables to join by
#' @param how the method used to join the datasets
#' @param suffix_l suffix for the original dataset (ignored for filter-joins)
#' @param suffix_r suffix for the imported dataset (ignored for filter-joins)
#'
#' @return joined dataset
#' @rdname join_data
#' @seealso \code{\link{code}}, \code{\link[dplyr]{mutate-joins}},
#'          \code{\link[dplyr]{filter-joins}}
#' @author Zhaoming Su
#' @export
join_data <- function(data_l, data_r, by = NULL,
                      how = c("inner", "left", "right", "full", "anti", "semi"),
                      suffix_l = ".x", suffix_r = ".y") {
    expr <- rlang::enexprs(data_l, data_r)
    how <- rlang::arg_match(how)
    join_fn <- rlang::parse_expr(sprintf("dplyr::%s_join", how))
    fn_arg <- rlang::list2(expr[[2]], by = rlang::enexpr(by))
    if (how %in% c("inner", "left", "right", "full")) {
        fn_arg <- c(fn_arg, suffix = rlang::expr(c(!!suffix_l, !!suffix_r)))
    }
    expr <- rlang::expr(!!expr[[1]] %>% (!!join_fn)(!!!fn_arg))
    if (is.null(by)) {
        by <- capture.output(data <- eval_code(expr), type = "message") |>
            paste(collapse = " ") |>
            stringr::str_replace(".+\\((.+)\\).+", "\\1") |>
            strsplit(", ") |>
            purrr::list_c() |>
            rlang::set_names()
    } else {
        data <- eval_code(expr)
    }
    structure(data, join_cols = by)
}
