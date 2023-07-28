#' Aggregate data by categorical variables
#'
#' Summarizes non-categorical variables in a dataframe by grouping them
#' based on specified categorical variables and returns the aggregated result
#' along with the tidyverse code used to generate it.
#'
#' @param data A dataframe or survey design object to be aggregated.
#' @param group_vars A character vector specifying the variables in `data` to '
#'        be used as grouping factors.
#' @param summaries An unnamed character vector or named list (with the names
#'        being the names of variables in the dataset to calculate summaries of,
#'        and the elements being character vectors) of summaries to generate
#'        for the groups generated in `group_vars` (see details).
#' @param summaries An unnamed character vector or named list of summary
#'        functions to calculate for each group.
#'        If unnamed, the vector elements should be names of variables in the
#'        dataset for which summary statistics need to be calculated.
#'        If named, the names should correspond to the summary functions
#'        (e.g., "mean", "sd", "iqr") to be applied to each variable.
#' @param vars (Optional) A character vector specifying the names of variables
#'        in the dataset for which summary statistics need to be calculated.
#'        This argument is ignored if `summaries` is a named list.
#' @param names (Optional) A character vector or named list providing name
#'        templates for the newly created variables. See details for more
#'        information.
#' @param quantiles (Optional) A numeric vector specifying the desired
#'        quantiles (e.g., c(0.25, 0.5, 0.75)).
#'        See details for more information.
#'
#' @return An aggregated dataframe containing the summary statistics for each
#'         group, along with the tidyverse code used for the aggregation.
#'
#' @rdname aggregate_data
#' @seealso \code{\link{code}}
#'
#' @details
#' The `aggregate_data()` function accepts any R function that returns a
#' single-value summary (e.g., `mean`, `var`, `sd`, `sum`, `IQR`). By default,
#' new variables are named `{var}_{fun}`, where `{var}` is the variable name
#' and `{fun}` is the summary function used. The user can provide custom names
#' using the `names` argument, either as a vector of the same length as `vars`,
#' or as a named list where the names correspond to summary functions (e.g.,
#' "mean" or "sd").
#'
#' The special summary "missing" can be included, which counts the number of
#' missing values in the variable. The default name for this summary is
#' `{var}_missing`.
#'
#' If `quantiles` are requested, the function calculates the specified
#' quantiles (e.g., 25th, 50th, 75th percentiles), creating new variables for
#' each quantile. To customize the names of these variables, use `{p}` as a
#' placeholder in the `names` argument, where `{p}` represents the quantile
#' value. For example, using `names = "Q{p}_{var}"` will create variables like
#' "Q0.25_Sepal.Length" for the 25th percentile.
#'
#' @examples
#' aggregated <-
#'     aggregate_data(iris,
#'         group_vars = c("Species"),
#'         summaries = c("mean", "sd", "iqr")
#'     )
#' code(aggregated)
#' head(aggregated)
#'
#' @author Tom Elliott, Owen Jin, Zhaoming Su
#' @export
#' @md
aggregate_data <- function(data, group_vars, summaries,
                           vars = NULL, names = NULL,
                           quantiles = c(0.25, 0.75)) {
    expr <- rlang::enexpr(data)
    summaries <- validate_agg_args(data, group_vars, summaries, vars)
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


validate_agg_args <- function(data, group_vars, summaries, vars) {
    if (!is.list(summaries)) {
        if (!is.null(names(summaries))) {
            rlang::abort("`summaries` must be an unnamed vector or named list.")
        }
        if (is.null(vars)) {
            cols <- names(data)[purrr::map_lgl(data, is_num)]
            if (length(cols) == 0) {
                rlang::abort("No numeric/date-time variables to aggregate.")
            }
            vars <- cols[!cols %in% group_vars]
        }
        summaries <- replicate(length(vars), summaries, simplify = FALSE) |>
            rlang::set_names(vars)
    } else {
        if (is.null(names(summaries)) || "" %in% names(summaries)) {
            rlang::abort("`summaries` must be an unnamed vector or named list.")
        }
        if (!is.null(vars)) {
            rlang::warn("`vars` is ignored and overridden by `summaries`.")
        }
    }
    summaries
}


smry_expr <- function(vars, summaries, names, quantiles, is_svy) {
    purrr::map(unique(purrr::list_c(summaries)), function(x) {
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
        purrr::map(names(summaries), function(var) {
            name <- (is_svy && x == "quantile") |>
                ifelse(gsub("\\_q\\{100\\*p\\}$", "", names[[x]]), names[[x]])
            if (!x %in% summaries[[var]]) {
                return(list(structure("", name = name)))
            }
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
        (\(x) rlang::set_names(x, purrr::map_chr(x, attr, "name")))() |>
        (\(x) x[!purrr::map_lgl(x, is.character)])()
}


make_varnames <- function(summaries, names) {
    summaries <- unique(purrr::list_c(summaries))
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

#' @describeIn aggregate_data Aggregate data by dates and times
#' @param dt A character string representing the name of the date-time variable
#'        in the dataset.
#' @param dt_comp A character string specifying the component of the date-time
#'        to use for grouping.
#' @author Zhaoming Su
#' @seealso \code{\link{aggregate_data}}
#' @md
#' @export
aggregate_dt <- function(data, dt, dt_comp, group_vars = NULL,
                         summaries, vars = NULL, names = NULL,
                         quantiles = c(0.25, 0.75)) {
    assign(rlang::expr_deparse(expr <- rlang::enexpr(data)), data)
    ._x_ <- rlang::inject(extract_dt_comp(!!expr, dt, dt_comp))
    agg <- aggregate_data(
        ._x_, c(sprintf("%s%s", dt, get_dt_comp(dt_comp)$suffix), group_vars),
        summaries, vars, names, quantiles
    )
    new_code <- gsub("\\._x_", paste(code(._x_), collapse = "\n"), code(agg))
    structure(agg, code = new_code)
}
