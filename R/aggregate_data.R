#' Aggregate data by categorical variables
#'
#' Aggregate a dataframe into summaries of all numeric variables by grouping
#' them by specified categorical variables
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param .data a dataframe to aggregate
#' @param vars  a character vector of categorical variables in \code{.data}
#'        to group by
#' @param summaries summaries to generate for the groups generated
#'        in \code{vars}.
#'        Valid summaries are "iqr" , mean", "median", "sd", "sum"
#' @return aggregated dataframe containing the summaries
#'         with tidyverse code attached
#' @seealso \code{\link{code}}
#' @seealso \code{\link{countMissing}}
#'
#' @examples
#' aggregated <-
#'     aggregateData(iris,
#'         vars = c("Species"),
#'         summaries = c("mean", "sd", "iqr")
#'     )
#' cat(code(aggregated))
#' head(aggregated)
#'
#' @author Owen Jin
#' @export
aggregateData <- function(.data, vars, summaries) {

    mc <- match.call()
    dataname <- mc$.data

    summaries <- tolower(summaries)

    summary_names <- summaries %>%
        sort() %>%
        c("missing")

    summaries_fun <-
        ifelse(summaries == "iqr", "IQR", summaries) %>%
            sort() %>%
            c("iNZightTools::countMissing")

    numeric_vars <- colnames(dplyr::select_if(.data, is.numeric)) %>%
        sort()

    # paste together the categorical variables for the group_by() statement
    groupby_str <- str_c(vars, collapse = ", ")
    # paste together all the numeric variables and what summaries are
    # requested for the summarize
    summarize_str <- str_c(
        rep(sort(numeric_vars), each = length(summary_names)),
        ".",
        rep(summary_names, length(numeric_vars)),
        " = ",
        rep(summaries_fun, length(numeric_vars)),
        "(",
        rep(sort(numeric_vars), each = length(summary_names)),
        ", na.rm = TRUE)",
        collapse = ", "
    )

    summarize_str <- str_c("count = dplyr::n(), ", summarize_str)

    exp <- ~.data %>%
        dplyr::group_by(.EVAL_GROUPBY) %>%
        dplyr::summarize(.EVAL_SUMMARIZE)

    exp <- replaceVars(exp,
        .EVAL_GROUPBY = groupby_str,
        .EVAL_SUMMARIZE = summarize_str,
        .data = dataname
    )

    interpolate(exp)
}
