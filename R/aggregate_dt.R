#' A wrapper function of `iNZightTools::aggregate_data()` for date-time grouping
#'
#' @param data a dataframe or survey design object to aggregate
#' @param dt a string of the name of the date-time variable
#' @param dt_comp a string of the component of the date-time to group by
#' @param summaries an unnamed character vector or named list (with the names
#'        being the names of variables in the dataset to calculate summaries of,
#'        and the elements being character vectors) of summaries to generate
#'        for the groups generated in `group_vars`
#' @param vars names of variables in the dataset to calculate summaries of
#'        (ignored if `summaries` is a named list)
#' @param names name templates for created variables
#' @param quantiles if requesting quantiles, specify the desired quantiles here
#' @return aggregated dataframe containing the summaries
#'         with tidyverse code attached
#' @rdname aggregate_dt
#' @author Zhaoming Su
#' @seealso \code{\link{aggregate_data}}
#' @md
#' @export
aggregate_dt <- function(data, dt, dt_comp, summaries,
                         vars = NULL, names = NULL,
                         quantiles = c(0.25, 0.75)) {
    assign(rlang::expr_deparse(expr <- rlang::enexpr(data)), data)
    ._x_ <- rlang::inject(extract_dt_comp(!!expr, dt, dt_comp))
    agg <- aggregate_data(
        ._x_, sprintf("%s%s", dt, get_dt_comp(dt_comp)$suffix),
        summaries, vars, names, quantiles
    )
    new_code <- gsub("\\._x_", paste(code(._x_), collapse = "\n"), code(agg))
    structure(agg, code = new_code)
}
