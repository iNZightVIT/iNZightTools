#' Form Class Intervals
#'
#' This function creates categorical intervals from a numeric variable in the given dataset.
#'
#' @param data A dataset or a survey object.
#' @param variable The name of the numeric variable to convert into intervals.
#' @param method The method used to create intervals:
#' - 'equal' for equal-width intervals,
#' - 'width' for intervals of a specific width,
#' - 'count' for equal-count intervals, and
#' - 'manual' to specify break points manually.
#' @param n_intervals For methods 'equal' and 'count', this specifies the
#'        number of intervals to create.
#' @param interval_width For method 'width', this sets the width of the
#'        intervals.
#' @param format The format for interval labels; use 'a' and 'b' to represent
#'        the min/max of each interval, respectively.
#' @param range The range of the data; use this to adjust the labels
#'        (e.g., for continuous data, set this to the floor/ceiling
#'        of the min/max of the data to get prettier intervals).
#'        If range does not cover the range of the data, values outside will be
#'        placed into 'less than a' and 'greater than b' categories.
#' @param format_lowest Label format for values lower than the min of range.
#' @param format_highest Label format for values higher than the max of range.
#' @param break_points For method 'manual', specify breakpoints here as a
#'        numeric vector.
#' @param name The name of the new variable in the resulting data set.
#' @return A dataframe with an additional column containing categorical class
#'         intervals.
#' @author Tom Elliott, Zhaoming Su
#' @md
#' @export
#' @examples
#' form_class_intervals(iris, "Sepal.Length", "equal", 5L)
form_class_intervals <- function(data, variable,
                                 method = c("equal", "width", "count", "manual"),
                                 n_intervals = 4L,
                                 interval_width,
                                 format = "(a,b]",
                                 range = NULL,
                                 format_lowest = ifelse(isinteger, "< a", "<= a"),
                                 format_highest = "> b",
                                 break_points = NULL,
                                 name = sprintf("%s.f", variable)) {
    # equal: equal-size intervals
    # width: intervals of a specified width
    # count: equal-count intervals
    # manual: as specified using `break_points`
    # if variable is an INTEGER, use [0, k-1], [k, 2k-1], ...
    # truncate range if range is specified: values outside range
    # put into a 'less than x' or 'x or higher' interval
    # based on formats specifed by `format_lowest` and `format_highest`.
    expr <- rlang::enexpr(data)
    method <- rlang::arg_match(method)
    .data <- if (is_survey(data)) data$variables else data
    x <- .data[[variable]]
    if (!is.null(range)) {
        xr <- range
    } else {
        xr <- range(x, na.rm = TRUE)
        xr <- floor(xr) + c(0, 1)
    }
    isinteger <- all(round(x) == x, na.rm = TRUE)
    if (isinteger) {
        format <- stringr::str_replace_all(format, "\\(", "[") |>
            stringr::str_replace_all("\\)", "]")
    }

    # 1. Generate BREAK points
    breaks_expr <- switch(method,
        "equal" = sprintf(
            "seq(%s, %s, length.out = %iL)",
            xr[1], xr[2], n_intervals + 1L
        ),
        "width" = sprintf(
            "seq(%s, %s, by = %d)",
            xr[1], xr[2], interval_width
        ),
        # use survey quantiles for survey data ...
        "count" = sprintf(
            "quantile(%s, probs = seq(0, 1, 1 / %i), na.rm = TRUE, type = 4)",
            variable, n_intervals
        ),
        "manual" = sprintf("%s", list(break_points))
    ) |> rlang::parse_expr()
    if (isinteger) breaks_expr <- rlang::expr(round(!!breaks_expr))
    breaks <- rlang::eval_tidy(rlang::expr(with(.data, !!breaks_expr)))

    # 2. Generate LABELS
    labels <- cbind(breaks[-length(breaks)], breaks[-1]) |>
        purrr::array_branch(1) |>
        purrr::map_chr(function(z) {
            if (isinteger && z[2] < max(x, na.rm = TRUE)) z[2] <- z[2] - 1
            sprintf(gsub("a|b", "%s", format), z[1], z[2])
        })
    new_breaks <- FALSE
    if (min(breaks) > min(x, na.rm = TRUE)) {
        labels <- c(gsub("a", breaks[1], format_lowest), labels)
        breaks <- c(-Inf, breaks)
        new_breaks <- TRUE
    }
    if (max(breaks) < max(x, na.rm = TRUE)) {
        labels <- c(labels, gsub("b", breaks[length(breaks)], format_highest))
        breaks <- c(breaks, Inf)
        new_breaks <- TRUE
    }
    labels[1] <- gsub("\\(", "[", labels[1])
    labels[length(labels)] <- gsub("\\)", "]", labels[length(labels)])

    # 3. Cut variable and place into data.frame
    vars_expr <- rlang::list2(!!name := rlang::parse_expr(sprintf(
        "cut(%s, breaks = %s, labels = %s, include.lowest = TRUE, right = %s)",
        variable,
        list(as.numeric(breaks)),
        list(as.character(labels)),
        grepl("]", format)
    )))
    expr <- mutate_expr_i(expr, vars_expr, data, .after = variable)
    eval_code(expr)
}
