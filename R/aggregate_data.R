#' Aggregate data by categorical variables
#'
#' Aggregate a dataframe into summaries of all numeric/date-time variables by
#' grouping them by specified categorical variables
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param data a dataframe or survey design object to aggregate
#' @param group_vars  a character vector of categorical variables in \code{data}
#'        to group by
#' @param summaries summaries to generate for the groups generated
#'        in \code{group_vars} (see details)
#' @param vars names of variables in the dataset to calculate summaries of
#' @param names name templates for created variables (see details)
#' @param quantiles if requesting quantiles, specify the desired quantiles here
#' @return aggregated dataframe containing the summaries
#'         with tidyverse code attached
#' @rdname aggregate_data
#' @seealso \code{\link{code}}
#'
#' @section Calculating variable summaries:
#' The `aggregate_data` function accepts any R function which returns a
#' single-value (such as `mean`, `var`, `sd`, `sum`, `IQR`). The default name of
#' new variables will be `{var}_{fun}`, where `{var}` is the variable name and
#' `{fun}` is the summary function used. You may pass new names via the `names`
#' argument, which should be either a vector the same length as `vars`, or a
#' named list (where the names are the summary function). In either case, use
#' `{var}` to represent the variable name. e.g., `{var}_mean` or `min_{var}`.
#'
#' You can also include the summary `missing`, which will count the number of
#' missing values in the variable. It has default name `{var}_missing`.
#'
#' For the `quantile` summary, there is the additional argument `quantiles`.
#' A new variable will be created for each specified quantile 'p'. To name these
#' variables, use `{p}` in `names` (the default is `{var}_q{p}`).
#'
#' @examples
#' aggregated <-
#'     aggregate_data(iris,
#'         group_vars = c("Species"),
#'         summaries = c("mean", "sd", "iqr")
#'     )
#' cat(code(aggregated))
#' head(aggregated)
#'
#' @author Tom Elliott, Owen Jin, Stephen Su
#' @export
#' @md
aggregate_data <- function(data, group_vars, summaries,
                           vars = NULL, names = NULL,
                           quantiles = c(0.25, 0.75)) {
    expr <- rlang::enexpr(data)
    if (is.null(vars)) {
        cols <- names(data)[purrr::map_lgl(data, is_num)]
        if (length(cols) == 0) {
            rlang::abort("No numeric/date-time variables to aggregate.")
        }
        vars <- cols[!cols %in% group_vars]
    }
    names <- make_varnames(summaries, names)
    vars_expr <- smry_expr(vars, summaries, names, quantiles, is_survey(data))
    if (is_survey(data)) {
        expr <- coerce_tbl_svy(expr, data)
    }
    s_fn <- sprintf("%s::summarise", ifelse(is_survey(data), "srvyr", "dplyr"))
    g_fn <- sprintf("%s::group_by", ifelse(is_survey(data), "srvyr", "dplyr"))
    expr <- rlang::expr(!!expr %>%
        (!!rlang::parse_expr(g_fn))(!!!rlang::syms(group_vars)) %>%
        (!!rlang::parse_expr(s_fn))(!!!vars_expr, .groups = "drop"))
    eval_code(expr)
}


smry_expr <- function(vars, summaries, names, quantiles, is_svy) {
    purrr::map(summaries, function(x) {
        qt <- if (x == "quantile") rlang::exprs(c(!!!quantiles)) else NULL
        na <- if (x == "count") NULL else list(na.rm = TRUE)
        svy_fn <- ifelse(is_svy, "srvyr::survey_", "")
        fn <- dplyr::case_match(x,
            "count" ~ ifelse(is_svy, "srvyr::survey_total", "dplyr::n"),
            "iqr" ~ ifelse(is_svy, "iNZightTools::survey_IQR", "IQR"),
            "sum" ~ ifelse(is_svy, "srvyr::survey_total", "sum"),
            .default = paste0(svy_fn, x)
        ) |> rlang::parse_expr()
        if (x == "count") {
            return(list(rlang::expr((!!fn)()) |>
                structure(name = glue::glue(names[[x]]))))
        }
        purrr::map(vars, function(var) {
            name <- (is_svy && x == "quantile") |>
                ifelse(gsub("\\_q\\{100\\*p\\}$", "", names[[x]]), names[[x]])
            if (!is_svy && x == "quantile") {
                s_expr <- rlang::exprs(
                    (!!fn)(!!rlang::sym(var), !!quantiles[1], !!!na),
                    (!!fn)(!!rlang::sym(var), !!quantiles[2], !!!na)
                )
                attr(s_expr[[1]], "name") <- glue::glue(name, p = quantiles[1])
                attr(s_expr[[2]], "name") <- glue::glue(name, p = quantiles[2])
                return(s_expr)
            } else if (x == "missing") {
                s_expr <- rlang::expr(sum(is.na(!!rlang::sym(var))))
            } else {
                s_expr <- rlang::expr((!!fn)(!!rlang::sym(var), !!!qt, !!!na))
            }
            structure(s_expr, name = glue::glue(name))
        }) |> purrr::list_flatten()
    }) |>
        purrr::list_flatten() |>
        (\(x) rlang::set_names(x, purrr::map_chr(x, attr, "name")))()
}


make_varnames <- function(summaries, names) {
    default_names <- purrr::map(summaries, agg_default_name) |>
        rlang::set_names(summaries)
    if (is.null(names)) {
        default_names
    } else if (length(names) != length(summaries)) {
        utils::modifyList(default_names, as.list(names))
    } else {
        rlang::set_names(as.list(names), summaries)
    }
}


agg_default_name <- function(x) {
    dplyr::case_match(x,
        "count" ~ "count",
        "quantile" ~ "{var}_q{100*p}",
        "missing" ~ "{var}_missing",
        .default = sprintf("{var}_%s", x)
    )
}


#' Interquartile range function for surveys
#'
#' Calculates the interquartile range from complex survey data.
#' A wrapper for taking differences of `svyquantile` at 0.25 and 0.75 quantiles,
#' and meant to be called from within `summarize` (see [srvyr] package).
#'
#' @param x A variable or expression
#' @param na.rm logical, if `TRUE` missing values are removed
#'
#' @return a vector of interquartile ranges
#'
#' @examples
#' library(survey)
#' library(srvyr)
#' data(api)
#'
#' dstrata <- apistrat %>%
#'     as_survey(strata = stype, weights = pw)
#'
#' dstrata %>%
#'     summarise(api99_iqr = survey_IQR(api99))
#'
#' @author Tom Elliott
#' @md
#' @export
survey_IQR <- function(x, na.rm = TRUE) {
    .svy <- srvyr::set_survey_vars(srvyr::cur_svy(), x)
    qs <- survey::svyquantile(~`__SRVYR_TEMP_VAR__`,
        quantiles = c(0.25, 0.75),
        se = FALSE,
        na.rm = na.rm, design = .svy
    )
    if (utils::packageVersion("survey") >= "4.1") {
        out <- sapply(qs, function(qx) diff(qx[, 1]))
    } else {
        out <- apply(qs, 1, diff)
    }
    out
}
