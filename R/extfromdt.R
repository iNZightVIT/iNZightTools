# Extract part of a datetimes variable
#' Extract part of a datetimes variable
#'
#' @param .data dataframe
#' @param varname name of the variable
#' @param part part of the variable wanted
#' @param name name of the new column
#'
#' @return dataframe with extracted part column
#' @export
#'
#' @author Yiwen He
#'
extract_part = function(.data, varname, part, name) {
  mc <- match.call()
  dataname <- mc$.data


  extexp = switch(part,
    "Date only" = "as.Date(.DATA$.VARNAME)",
    "Year" = 'as.numeric(format(.DATA$.VARNAME, "%C%y"))',
    "Century" = 'as.numeric(format(.DATA$.VARNAME, "%C"))',
    "Decimal Year" = 'lubridate::decimal_date(.DATA$.VARNAME)',
    "Year Quarter" = 'factor(as.character(zoo::as.yearqtr(.DATA$.VARNAME)))',
    "Quarter" = 'as.numeric(stringr::str_sub(zoo::as.yearqtr(.DATA$.VARNAME), -1))',
    "Year Month" = 'factor(format(.DATA$.VARNAME, "%Y M%m"))',
    "Month (full)" = 'factor(format(.DATA$.VARNAME, "%B"), levels = MONTHS_FULL)',
    "Month (abbreviated)" = 'factor(format(.DATA$.VARNAME, "%b"), levels = MONTHS_ABBR)',
    "Month (number)" = 'as.numeric(format(.DATA$.VARNAME, "%m"))',
    "Year Week" = 'factor(format(.DATA$.VARNAME, "%Y W%W"))',
    "Week of the year (Monday as first day of the week)" = 'as.numeric(format(.DATA$.VARNAME, "%W"))',
    "Week of the year (Sunday as first day of the week)" = 'as.numeric(format(.DATA$.VARNAME, "%U"))',
    "Day of the year" = 'as.numeric(format(.DATA$.VARNAME, "%j"))',
    "Day of the week (name)" = 'factor(format(.DATA$.VARNAME, "%A"))',
    "Day of the week (abbreviated)" = 'factor(lubridate::wday(.DATA$.VARNAME, label = TRUE))',
    "Day of the week (number, Monday as 1)" = 'as.numeric(format(.DATA$.VARNAME, "%u"))',
    "Day of the week (number, Sunday as 0)" = 'as.numeric(format(.DATA$.VARNAME, "%w"))',
    "Day" = 'as.numeric(format(.DATA$.VARNAME, "%d"))',
    "Time only" = 'chron::chron(times. = format(.DATA$.VARNAME, "%H:%M:%S"))',
    "Hours (decimal)" = 'lubridate::hour(.DATA$.VARNAME) + (lubridate::minute(.DATA$.VARNAME) + lubridate::second(.DATA$.VARNAME) / 60) /60',
    "Hour" = 'as.numeric(format(.DATA$.VARNAME, "%H"))',
    "Minute" = 'as.numeric(format(.DATA$.VARNAME, "%M"))',
    "Second" = 'as.numeric(format(.DATA$.VARNAME, "%S"))')

  exp = ~.DATA %>%
    tibble::add_column(.NAME = .EXTEXP, .after = ".VARNAME")


  exp = replaceVars(exp,
                    .EXTEXP = extexp,
                    .DATA = dataname,
                    .NAME = name,
                    .VARNAME = varname)

  if (grepl("MONTHS_(FULL|ABBR)", extexp)) {
    months <- as.Date(paste("2019", 1:12, "01", sep = "-"))
    if (grepl("MONTHS_FULL", extexp))
        exp <- replaceVars(exp, MONTHS_FULL = format(months, "%B"))
    else
        exp <- replaceVars(exp, MONTHS_ABBR = format(months, "%b"))
  }

  interpolate(exp)
}
