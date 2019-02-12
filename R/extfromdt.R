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
  
  if (part == "Hours (decimal)") {
    Fname = strsplit(format(.DATA$.VARNAME, "%H:%M:%S"), ":")
    for (i in 1:length(Fname)) {
      xyz = as.numeric(Fname[[i]][1]) + (as.numeric(Fname[[i]][2]) + as.numeric(Fname[[i]][3]) * 60) / 60
    }
    exp = ~.DATA %>%
      tibble::add_column(.NAME = xyz, .after = ".VARNAME")
  } else {
    extexp = switch(part, "Date only" = "as.Date(.DATA$.VARNAME)",
                    "Year" = 'format(.DATA$.VARNAME, "%C%y")',
                    "Century" = 'format(.DATA$.VARNAME, "%C")',
                    "Decimal Year" = 'lubridate::decimal_date(.DATA$.VARNAME)',
                    "Year Quarter" = 'zoo::as.yearqtr(.DATA$.VARNAME)',
                    "Quarter" = 'stringr::str_sub(zoo::as.yearqtr(.DATA$.VARNAME), -1)',
                    "Year Month" = 'format(.DATA$.VARNAME, "%Y M%m")',
                    "Month (full)" = 'format(.DATA$.VARNAME, "%B")',
                    "Month (abbreviated)" = 'format(.DATA$.VARNAME, "%b")',
                    "Month (number)" = 'format(.DATA$.VARNAME, "%m")',
                    "Week of the year (Monday as first day of the week)" = 'format(.DATA$.VARNAME, "%W")',
                    "Week of the year (Sunday as first day of the week)" = 'format(.DATA$.VARNAME, "%U")',
                    "Day of the year" = 'format(.DATA$.VARNAME, "%j")',
                    "Day of the week (name)" = 'format(.DATA$.VARNAME, "%A")',
                    "Day of the week (number, Monday as 1)" = 'format(.DATA$.VARNAME, "%u")',
                    "Day of the week (number, Sunday as 0)" = 'format(.DATA$.VARNAME, "%w")',
                    "Day" = 'format(.DATA$.VARNAME, "%d")',
                    "Time only" = 'format(.DATA$.VARNAME, "%H:%M:%S")',
                    "Hour" = 'format(.DATA$.VARNAME, "%H")',
                    "Minute" = 'format(.DATA$.VARNAME, "%M")',
                    "Second" = 'format(.DATA$.VARNAME, "%S")'
    )
    exp = ~.DATA %>%
      tibble::add_column(.NAME = .EXTEXP, .after = ".VARNAME")
  }

  exp = replaceVars(exp, 
                    .EXTEXP = extexp, 
                    .DATA = dataname, 
                    .NAME = name, 
                    .VARNAME = varname)
  
  interpolate(exp)
  
}


  # extexp = switch(part, "Date only" = "as.Date(.DATA$.VARNAME)",
  #                 "Year" = 'format(.DATA$.VARNAME, "%C%y")',
  #                 "Century" = 'format(.DATA$.VARNAME, "%C")',
  #                 "Year Quarter" = 'zoo::as.yearqtr(.DATA$.VARNAME)',
  #                 "Quarter" = 'stringr::str_sub(zoo::as.yearqtr(.DATA$.VARNAME), -1)',
  #                 "Year Month" = 'format(.DATA$.VARNAME, "%Y M%m")',
  #                 "Month (full)" = 'format(.DATA$.VARNAME, "%B")',
  #                 "Month (abbreviated)" = 'format(.DATA$.VARNAME, "%b")',
  #                 "Month (number)" = 'format(.DATA$.VARNAME, "%m")',
  #                 "Week of the year" = 'format(.DATA$.VARNAME, "%W")',
  #                 "Day of the year" = 'format(.DATA$.VARNAME, "%j")',
  #                 "Day of the week (name)" = 'format(.DATA$.VARNAME, "%A")',
  #                 "Day of the week (number)" = 'format(.DATA$.VARNAME, "%u")',
  #                 "Day" = 'format(.DATA$.VARNAME, "%d")',
  #                 "Time only" = 'format(.DATA$.VARNAME, "%H:%M:%S")',
  #                 "Hour" = 'format(.DATA$.VARNAME, "%H")',
  #                 "Minute" = 'format(.DATA$.VARNAME, "%M")',
  #                 "Second" = 'format(.DATA$.VARNAME, "%S")'
  # )

  # exp = ~.DATA %>%
  #   tibble::add_column(.NAME = .EXTEXP, .after = ".VARNAME")
  # 
  # exp = replaceVars(exp, 
  #                   .EXTEXP = extexp, 
  #                   .DATA = dataname, 
  #                   .NAME = name, 
  #                   .VARNAME = varname)
  # 
  # interpolate(exp)

## zoo package
## add hours(decimal)


# if (part == "Hours (decimal)") {
#   Fname = strsplit(format(.DATA$.VARNAME, "%H:%M:%S"), ":")
#   for (i in 1:length(Fname)) {
#     xyz = as.numeric(Fname[[i]][1]) + (as.numeric(Fname[[i]][2]) + as.numeric(Fname[[i]][3]) * 60) / 60
#   }
# }




