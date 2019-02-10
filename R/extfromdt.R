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
# extract_part = function(data, varname, part, name) {
#   varx = data[[varname]]
#   if (part == "Date only") {
#     var.dt = as.Date(varx)
#   }
#   if (part == "Year") {
#     var.dt = format(varx, "%C%y")
#   }
#   if (part == "Century") {
#     var.dt = format(varx, "%C")
#   }
#   if (part == "Year Quarter") {
#     var.dt = zoo::as.yearqtr(varx)
#   }
#   if (part == "Quarter") {
#     var.dt = zoo::as.yearqtr(varx)
#     var.dt = stringr::str_sub(var.dt, -1)
#   }
#   if (part == "Year Month") {
#     var.dt = format(varx, "%Y M%m")
#   }
#   if (part == "Month (full)") {
#     var.dt = format(varx, "%B")
#   }
#   if (part == "Month (abbreviated)") {
#     var.dt = format(varx, "%b")
#   }
#   if (part == "Month (number)") {
#     var.dt = format(varx, "%m")
#   }
#   if (part == "Week of the year") {
#     var.dt = format(varx, "%W")
#   }
#   if (part == "Day of the year") {
#     var.dt = format(varx, "%j")
#   }
#   if (part == "Day of the week (name)") {
#     var.dt = format(varx, "%A")
#   }
#   if (part == "Day of the week (number)") {
#     var.dt = format(varx, "%u")
#   }
#   if (part == "Day"){
#     var.dt = format(varx, "%d")
#   }
#   if (part == "Time only"){
#     var.dt = format(varx, "%H:%M:%S")
#   }
#   if (part == "Hours (decimal)") {
#     x = format(varx, "%H:%M:%S")
#     var.dt = sapply(strsplit(x, ":"), function(x) {
#       x = as.numeric(x)
#       x[1]+(x[2]+x[3]*60)/60
#     })
#   }
#   if (part == "Hour") {
#     var.dt = format(varx, "%H")
#   }
#   if (part == "Minute") {
#     var.dt = format(varx, "%M")
#   }
#   if (part == "Second") {
#     var.dt = format(varx, "%S")
#   }
#   exp = tibble::add_column(data, name = var.dt)
#   names(exp)[length(names(exp))] = name
#   return(exp)
# }

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
                    "Year Quarter" = 'zoo::as.yearqtr(.DATA$.VARNAME)',
                    "Quarter" = 'stringr::str_sub(zoo::as.yearqtr(.DATA$.VARNAME), -1)',
                    "Year Month" = 'format(.DATA$.VARNAME, "%Y M%m")',
                    "Month (full)" = 'format(.DATA$.VARNAME, "%B")',
                    "Month (abbreviated)" = 'format(.DATA$.VARNAME, "%b")',
                    "Month (number)" = 'format(.DATA$.VARNAME, "%m")',
                    "Week of the year" = 'format(.DATA$.VARNAME, "%W")',
                    "Day of the year" = 'format(.DATA$.VARNAME, "%j")',
                    "Day of the week (name)" = 'format(.DATA$.VARNAME, "%A")',
                    "Day of the week (number)" = 'format(.DATA$.VARNAME, "%u")',
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




