#' Extract part of a datetimes variable
#'
#' @param .data dataframe
#' @param varname name of the variable
#' @param part part of the variable wanted
#' @param name name of the new column
#'
#' @return dataframe with extracted part column
#' @author Yiwen He
#' @importFrom zoo as.yearqtr
#' @export
extract_part <- function(.data, varname, part, name) {
    mc <- match.call()
    dataname <- mc$.data

    extexp <- switch(part,
        "Date" = ,
        "Date only" =
            "as.Date(.DATA$.VARNAME)",
        "Year" =
            'as.numeric(format(.DATA$.VARNAME, "%C%y"))',
        "Century" =
            'as.numeric(format(.DATA$.VARNAME, "%C"))',
        "Decimal Year" =
            "lubridate::decimal_date(.DATA$.VARNAME)",
        "Year Quarter" =
            "factor(format(zoo::as.yearqtr(.DATA$.VARNAME), '%YQ%q'))",
        "Quarter" =
            "as.numeric(stringr::str_sub(zoo::as.yearqtr(.DATA$.VARNAME), -1))",
        "Year Month" =
            'factor(format(.DATA$.VARNAME, "%YM%m"))',
        "Month" = ,
        "Month (full)" =
            "lubridate::month(.DATA$.VARNAME, label = TRUE, abbr = FALSE)",
        "Month (abbreviated)" =
            "lubridate::month(.DATA$.VARNAME, label = TRUE)",
        "Month (number)" =
            'as.numeric(format(.DATA$.VARNAME, "%m"))',
        "Year Week" =
            'factor(format(.DATA$.VARNAME, "%YW%W"))',
        "Week" = ,
        "Week of the year (Monday as first day of the week)" =
            'as.numeric(format(.DATA$.VARNAME, "%W"))',
        "Week of the year (Sunday as first day of the week)" =
            'as.numeric(format(.DATA$.VARNAME, "%U"))',
        "Day of the year" =
            'as.numeric(format(.DATA$.VARNAME, "%j"))',
        "Day of the week (name)" =
            paste(
                sep = ", ",
                "lubridate::wday(.DATA$.VARNAME, label = TRUE",
                "abbr = FALSE, week_start = 1)"
            ),
        "Day of the week (abbreviated)" =
            "lubridate::wday(.DATA$.VARNAME, label = TRUE, week_start = 1)",
        "Day of the week (number, Monday as 1)" =
            'as.numeric(format(.DATA$.VARNAME, "%u"))',
        "Day of the week (number, Sunday as 0)" =
            'as.numeric(format(.DATA$.VARNAME, "%w"))',
        "Day" =
            'as.numeric(format(.DATA$.VARNAME, "%d"))',
        "Time" = ,
        "Time only" = {
            if (!requireNamespace("chron", quietly = TRUE)) {
                stop("Please install suggested package: 'chron'") # nocov
            }
            'chron::chron(times. = format(.DATA$.VARNAME, "%H:%M:%S"))'
        },
        "Hours (decimal)" =
            paste(
                sep = " + ",
                "lubridate::hour(.DATA$.VARNAME)",
                "(lubridate::minute(.DATA$.VARNAME)",
                "lubridate::second(.DATA$.VARNAME) / 60) /60"
            ),
        "Hour" =
            'as.numeric(format(.DATA$.VARNAME, "%H"))',
        "Minute" =
            'as.numeric(format(.DATA$.VARNAME, "%M"))',
        "Second" =
            'as.numeric(format(.DATA$.VARNAME, "%S"))'
    )

    exp <- ~ .DATA %>%
        tibble::add_column(.NAME = .EXTEXP, .after = ".VARNAME")

    exp <- replaceVars(exp,
        .EXTEXP = extexp,
        .DATA = dataname,
        .NAME = name,
        .VARNAME = varname
    )

    interpolate(exp)
}
