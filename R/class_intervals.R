#' Form Class Intervals
#'
#' Create categorical intervals from a numeric variable.
#'
#' @param .data the data set
#' @param variable name of the variable to convert
#' @param method one of 'equal' for equal-width intervals, 'width' for intervals of a specific width, 'count' for equal-count intervals, and 'manual' to specify break points manually
#' @param n_intervals for methods 'equal' and 'count', this is the number of intervals to create
#' @param interval_width for method 'width', this is the width of intervals
#' @param format the format for intervals; use 'a' and 'b' to represent the min/max of each interval, respectively.
#' @param range the range of the data; use this to adjust the labels (e.g., for continuous data, set this to floor/ceiling of the min/max of the data to get prettier intervals). If `range` does not cover the range of the data, values outside will be placed into 'less than a' and 'greater than b' categories
#' @param format.lowest values lower than the min of `range` will have this label format
#' @param format.highest values higher than the max of `range` will have this label format
#' @param break_points for `method` 'manual', specify breakpoints here (as a numeric vector)
#' @param name the name of the new variable in the resulting data set
#' @return a dataframe with an additional column with categorical class intervals
#' @md
#' @author Tom Elliott
#' @export
#' @examples
#' form_class_intervals(iris, 'Sepal.Length', 'equal', 5L)
form_class_intervals <- function(.data, variable,
                                 method = c("equal", "width", "count", "manual"),
                                 n_intervals = 4L,
                                 interval_width,
                                 format = "(a,b]",
                                 range = NULL,
                                 format.lowest = ifelse(isinteger, "< a", "<= a"),
                                 format.highest = "> b",
                                 break_points = NULL,
                                 name = sprintf("%s.f", variable)) {
    # equal: equal-size intervals
    # width: intervals of a specified width
    # count: equal-count intervals
    # manual: as specified using `break_points`

    # if variable is an INTEGER, use [0, k-1], [k, 2k-1], ...

    # tuncate range if range is specified: values outside range
    # put into a 'less than x' or 'x or higher' interval
    # based on formats specifed by `format.lowest` and `format.highest`.

    mc <- match.call()
    dataname <- mc$.data
    method <- match.arg(method)

    x <- .data[[variable]]
    xr <- range(x, na.rm = TRUE)
    if (!is.null(range)) xr <- range else {
        xr[1] <- floor(xr[1])
        xr[2] <- ceiling(xr[2])
    }
    isinteger <- all(round(x) == x, na.rm = TRUE)
    if (isinteger) format <- gsub("(", "[", gsub(")", "]", format, fixed = TRUE), fixed = TRUE)

    # 1. Generate BREAK points - this algorithm can be improved separately; should return an expression that, evaluated, gives break points
    breaks_expr <- switch(method,
        "equal" =
            sprintf("seq(%s, %s, length.out = %iL)",
                xr[1], xr[2], n_intervals + 1L),
        "width" =
            sprintf("seq(%s, %s, by = %d)",
                xr[1], xr[2], interval_width),
        # use survey quantiles for survey data ...
        "count" =
            sprintf("quantile(%s, probs = seq(0, 1, 1 / %i), na.rm = TRUE, type = 4)",
                variable, n_intervals),
        "manual" = sprintf("%s", list(break_points))
    )
    if (isinteger) breaks_expr <- sprintf("round(%s)", breaks_expr)
    breaks <- eval(parse(text = breaks_expr), envir = .data)

    # 2. Generate LABELS
    labels <- apply(
        cbind(breaks[-length(breaks)], breaks[-1]),
        1,
        function(z) {
            if (isinteger && z[2] < max(x, na.rm = TRUE)) z[2] <- z[2] - 1
            gsub("a", z[1], gsub("b", z[2], format))
        }
    )

    new_breaks <- FALSE
    if (min(breaks) > min(x, na.rm = TRUE)) {
        labels <- c(gsub("a", breaks[1], format.lowest), labels)
        breaks <- c(-Inf, breaks)
        new_breaks <- TRUE
    }
    if (max(breaks) < max(x, na.rm = TRUE)) {
        labels <- c(labels, gsub("b", breaks[length(breaks)], format.highest))
        breaks <- c(breaks, Inf)
        new_breaks <- TRUE
    }
    # if (new_breaks && )
    # breaks_expr <- sprintf("c(%s%s%s)",
    #     ifelse(min(breaks) == -Inf, "-Inf, ", ""),
    #     breaks_expr,
    #     ifelse(max(breaks) == Inf, ", Inf", "")
    # )

    labels[1] <- gsub("(", "[", labels[1], fixed = TRUE)
    labels[length(labels)] <- gsub(")", "]", labels[length(labels)], fixed = TRUE)


    # 3. Cut variable and place into data.frame
    fmla <- sprintf(
        ".NAME = cut(%s, breaks = %s, labels = %s, include.lowest = TRUE, right = %s)",
        ifelse(is_survey(.data), variable, sprintf(".DATA$%s", variable)),
        list(as.numeric(breaks)),
        list(as.character(labels)),
        grepl("]", format)
    )

    if (is_survey(.data)) {
        exp <- ~.DATA %>% update(.FMLA)
    } else {
        exp <- ~.DATA %>% tibble::add_column(.FMLA, .after = ".VARNAME")
    }
    exp <- replaceVars(exp,
        .FMLA = fmla,
        .DATA = dataname ,
        .VARNAME = variable,
        .NAME = name
    )

    interpolate(exp)
}
