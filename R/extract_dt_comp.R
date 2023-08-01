#' Extract date component from a date-time variable
#'
#' This function extracts a specific date component from a date-time variable
#' in a dataframe.
#'
#' @param data The dataframe containing the date-time variable.
#' @param var The name of the date-time variable to extract the component.
#' @param comp The date component wanted from the variable. See
#'        \code{iNZightTools:::inz_dt_comp} for the full list of components.
#' @param name The name of the new column to store the extracted date
#'        component.
#'
#' @return A dataframe with the new date component column.
#' @author Zhaoming Su
#' @export
extract_dt_comp <- function(data, var, comp, name = NULL) {
    expr <- rlang::enexpr(data)
    if (is.null(name)) {
        name <- sprintf("%s%s", var, get_dt_comp(comp)$suffix)
    }
    vars_expr <- rlang::list2(
        !!name := rlang::parse_expr(gsub("_x_", var, get_dt_comp(comp)$expr))
    )
    expr <- mutate_expr_i(expr, vars_expr, data, .after = var)
    eval_code(expr)
}

#' Extract part of a datetimes variable (DEPRECATED)
#'
#' This function has been replaced by `extract_dt_comp` and will be removed in the next release.
#'
#' @param .data dataframe
#' @param varname name of the variable
#' @param part part of the variable wanted
#' @param name name of the new column
#' @return see `extract_dt_comp`
#' @export
extract_part <- function(.data, varname, part, name) {
    extract_dt_comp(.data, varname, part, name)
}

## External use only for the `iNZight` package
## For making `offspring.data` list in `gtree`
get_dt_comp_tree <- function(x) {
    tree_list <- list()
    purrr::map(names(x), function(comp) {
        pos <- rlang::inject(paste(!!!x[[comp]]$ui, comp, sep = "\"]][[\""))
        sprintf("tree_list[[\"%s\"]] <<- comp", pos) |>
            rlang::parse_expr() |>
            rlang::eval_tidy()
    })
    tree_list
}


inz_dt_comp <- list(
    "Date only" = list(
        suffix = ".date",
        expr = "lubridate::as_date(_x_)",
        ui = "Date"
    ),
    "Decimal Year" = list(
        suffix = ".year",
        expr = "lubridate::year(_x_)",
        ui = c("Date", "Year")
    ),
    "Century" = list(
        suffix = ".century",
        expr = "as.numeric(format(_x_, \"%C\")) + 1",
        ui = c("Date", "Year")
    ),
    "Year Quarter" = list(
        suffix = ".yearquarter",
        expr = "tsibble::yearquarter(_x_)",
        ui = c("Date", "Quarter")
    ),
    "Decimal Quarter" = list(
        suffix = ".quarter",
        expr = "lubridate::quarter(_x_)",
        ui = c("Date", "Quarter")
    ),
    "Year Month" = list(
        suffix = ".yearmonth",
        expr = "tsibble::yearmonth(_x_)",
        ui = c("Date", "Month")
    ),
    "Month (full)" = list(
        suffix = ".month",
        expr = "lubridate::month(_x_, label = TRUE, abbr = FALSE)",
        ui = c("Date", "Month")
    ),
    "Month (abbreviated)" = list(
        suffix = ".month",
        expr = "lubridate::month(_x_, label = TRUE)",
        ui = c("Date", "Month")
    ),
    "Month (number)" = list(
        suffix = ".month",
        expr = "lubridate::month(_x_)",
        ui = c("Date", "Month")
    ),
    "Year Week" = list(
        suffix = ".yearweek",
        expr = "tsibble::yearweek(_x_)",
        ui = c("Date", "Week")
    ),
    "Week of the year (Monday as first day of the week)" = list(
        suffix = ".week",
        expr = "as.numeric(format(_x_, \"%W\"))",
        ui = c("Date", "Week")
    ),
    "Week of the year (Sunday as first day of the week)" = list(
        suffix = ".week",
        expr = "as.numeric(format(_x_, \"%U\"))",
        ui = c("Date", "Week")
    ),
    "Day of the month" = list(
        suffix = ".mday",
        expr = "lubridate::mday(_x_)",
        ui = c("Date", "Day")
    ),
    "Day of the year" = list(
        suffix = ".yday",
        expr = "lubridate::yday(_x_)",
        ui = c("Date", "Day")
    ),
    "Day of the week (name)" = list(
        suffix = ".wday",
        expr = "lubridate::wday(_x_, label = TRUE, abbr = FALSE, week_start = 1)",
        ui = c("Date", "Day")
    ),
    "Day of the week (abbreviated)" = list(
        suffix = ".wday",
        expr = "lubridate::wday(_x_, label = TRUE, week_start = 1)",
        ui = c("Date", "Day")
    ),
    "Day of the week (number, Monday as 1)" = list(
        suffix = ".wday",
        expr = "lubridate::wday(_x_, week_start = 1)",
        ui = c("Date", "Day")
    ),
    "Day of the week (number, Sunday as 0)" = list(
        suffix = ".wday",
        expr = "lubridate::wday(_x_, week_start = 7)",
        ui = c("Date", "Day")
    ),
    "Time only" = list(
        suffix = ".time",
        expr = "chron::chron(times. = format(_x_, \"%H:%M:%S\"))",
        ui = "Time"
    ),
    "Hour" = list(
        suffix = ".hour",
        expr = "lubridate::hour(_x_)",
        ui = "Time"
    ),
    "Hours (decimal)" = list(
        suffix = ".hours",
        expr = paste(
            "lubridate::hour(_x_)",
            "lubridate::minute(_x_) / 60",
            "lubridate::second(_x_) / 3600",
            sep = " + "
        ),
        ui = "Time"
    ),
    "Minute" = list(
        suffix = ".minute",
        expr = "lubridate::minute(_x_)",
        ui = "Time"
    ),
    "Second" = list(
        suffix = ".second",
        expr = "lubridate::second(_x_)",
        ui = "Time"
    )
)


get_dt_comp <- function(x) {
    opt_pkg <- c("chron", "tsibble", "lubridate") |>
        (\(p) p[purrr::map_lgl(p, grepl, x)])()
    if (length(opt_pkg) && !requireNamespace(opt_pkg, quietly = TRUE)) {
        rlang::abort(sprintf("Please install suggested package: '%s'", opt_pkg)) # nocov
    }
    x <- gsub("\\(", "\\\\(", gsub("\\)", "\\\\)", x))
    i <- which(grepl(sprintf("^%s", x), names(inz_dt_comp), TRUE))
    inz_dt_comp[[i[1]]]
}
