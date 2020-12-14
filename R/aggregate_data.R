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
#'        in \code{vars}. See details.
#' @param summary_vars names of variables in the dataset to calculate summaries of
#' @param varnames name templates for created variables (see details).
#' @param quantiles if requesting quantiles, specify the desired quantiles here
#' @param custom_funs a list of custom functions (see details).
#' @return aggregated dataframe containing the summaries
#'         with tidyverse code attached
#' @seealso \code{\link{code}}
#' @seealso \code{\link{countMissing}}
#'
#' @section Calculating variable summaries:
#' The `aggregateData` function accepts any R function which returns a single-value (such as `mean`, `var`, `sd`, `sum`, `IQR`). The default name of new variables will be `{var}_{fun}`, where `{var}` is the variable name and `{fun}` is the summary function used. You may pass new names via the `varnames` argument, which should be either a vector the same length as `summary_vars`, or a named list (where the names are the summary function). In either case, use `{var}` to represent the variable name. e.g., `{var}_mean` or `min_{var}`.
#'
#' You can also include the summary `missing`, which will count the number of missing values in the variable. It has default name `{var}_missing`.
#'
#' For the `quantile` summary, there is the additional argument `quantiles`. A new variable will be created for each specified quantile 'p'. To name these variables, use `{p}` in `varnames` (the default is `{var}_q{p}`).
#'
#' Custom functions can be passed via the `custom_funs` argument. This should be a list, and each element should have a `name` and either an `expr` or `fun` element. Expressions should operate on a variable `x`. The function should be a function of `x` and return a single value.
#' ```r
#' cust_funs <- list(name = '{var}_width', expr = diff(range(x), na.rm = TRUE))
#' cust_funs <- list(name = '{var}_stderr',
#'   fun = function(x) {
#'     s <- sd(x)
#'     n <- length(x)
#'     s / sqrt(n)
#'   }
#' )
#' ```
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
#' @author Tom Elliott, Owen Jin
#' @export
#' @md
aggregateData <- function(.data, vars, summaries,
                          summary_vars,
                          varnames = NULL,
                          quantiles = c(0.25, 0.75),
                          custom_funs = NULL) {

    mc <- match.call()
    dataname <- mc$.data

    if (missing(vars)) stop("Variables to aggregate over required")

    if (missing(summary_vars)) {
        cols <- colnames(.data)
        summary_vars <- cols[cols %notin% vars]
    }

    varnames <- make_varnames(summaries, varnames)
    summary_funs <- do.call(rbind,
        lapply(summaries,
            function(smry) {
                name <- varnames[[smry]]
                # functions that don't take na.rm argument
                na <- ifelse(smry %in% c("count"), "", ", na.rm = TRUE")
                fun <- switch(smry,
                    "count" = "dplyr::n()",
                    "iqr" = "IQR({var}, na.rm = TRUE)",
                    "missing" = "iNZightTools::countMissing({var})",
                    paste0(smry, "({var}", na, ")")
                )

                var <- summary_vars
                if (smry == "quantile") {
                    z <- do.call(rbind,
                        lapply(quantiles,
                            function(p) {
                                n <- glue::glue(name)
                                f <- glue::glue("quantile({var}, probs = {p / 100}, na.rm = TRUE)")
                                data.frame(n = n, f = f)
                            }
                        )
                    )

                    name <- z[,1]
                    fun <- z[,2]
                } else {
                    name <- glue::glue(name)
                    fun <- glue::glue(fun)
                }
                data.frame(name, fun)
            }
        )
    )

    smry_expr <- paste(summary_funs$name, summary_funs$fun, sep = " = ", collapse = ",\n")
    groupby_str <- stringr::str_c(vars, collapse = ", ")

    exp <- ~.data %>%
        dplyr::group_by(.EVAL_GROUPBY) %>%
        dplyr::summarize(
            .EVAL_SUMMARIZE
        )

    exp <- replaceVars(exp,
        .EVAL_GROUPBY = groupby_str,
        .EVAL_SUMMARIZE = smry_expr,
        .data = dataname
    )

    interpolate(exp)
}

make_varnames <- function(summaries, varnames) {
    default_varnames <- lapply(summaries, agg_default_name)
    names(default_varnames) <- summaries
    if (missing(varnames) || is.null(varnames)) {
        varnames <- default_varnames
    } else if (length(varnames) != length(summaries)) {
        varnames <- utils::modifyList(default_varnames, varnames)
    } else {
        varnames <- as.list(varnames)
        names(varnames) <- summaries
    }
    varnames
}

agg_default_name <- function(fun) {
    switch(fun,
        "count" = "count",
        "quantile" = "{var}_q{p}",
        "missing" = "{var}_missing",
        paste("{var}", fun, sep = "_")
    )
}
