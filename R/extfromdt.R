# Extract part of a datetimes variable
#' Extract part of a datetimes variable
#'
#' @param data dataframe
#' @param varname name of the variable
#' @param part part of the variable wanted
#' @param name name of the new column
#'
#' @return dataframe with extracted part column
#' @export
#'
#' @author Yiwen He
extract_part = function(data, varname, part, name) {
  varx = data[[varname]]
  if (part == "Year") {
    var.dt = lubridate::year(varx)
  }
  if (part == "Month (abbreviated)") {
    var.dt = lubridate::month(varx, label = TRUE, abbr = TRUE)
  }
  if (part == "Month (full)") {
    var.dt = lubridate::month(varx, label = TRUE)
  }
  if (part == "Day"){
    var.dt = lubridate::day(varx)
  }
  if (part == "Year quarter") {
    var.dt = zoo::as.yearqtr(varx)
  }
  if (part == "Quarter") {
    var.dt = zoo::as.yearqtr(varx)
    var.dt = stringr::str_sub(var.dt, -1)
  }
  if (part == "Year month") {
    year = lubridate::year(varx)
    month = sprintf("%02d", lubridate::month(varx))
    var.dt = paste(year, " M", month, sep ="")
  }
  if (part == "Week of the year") {
    var.dt = lubridate::week(varx)
  }
  if (part == "Day of the year") {
    var.dt = lubridate::yday(varx)
  }
  if (part == "Day of the week (number)") {
    var.dt = lubridate::wday(varx)
  }
  if (part == "Time only"){
    var.dt = chron::times(strftime(varx, "%H:%M:%S", tz = "UTC"))
    var.dt = as.character(var.dt)
    if (all(var.dt == "00:00:00")) {
      var.dt = NA
    }
  }
  if (part == "Date only") {
    var.dt = as.Date(varx)
  }
  if (part == "Month (number)") {
    var.dt = lubridate::month(varx)
  }
  if (part == "Day of the week (name)") {
    var.dt = lubridate::wday(varx, label = TRUE)
  }
  if (part == "Hours (decimal)") {
    x = chron::times(strftime(varx, "%H:%M:%S", tz = "UTC"))
    x = as.character(x)
    if (all(x == "00:00:00")) {
      var.dt = NA
    } else {
      var.dt = sapply(strsplit(x, ":"), function(x) {
        x = as.numeric(x)
        x[1]+(x[2]+x[3]*60)/60
      })
    }
  }
  if (part == "Hours") {
    var.dt = lubridate::hour(varx)
    if (all(var.dt == "0")) {
      var.dt = NA
    }
  }
  if (part == "Minutes") {
    var.dt = lubridate::minute(varx)
    if (all(var.dt == "0")) {
      var.dt = NA
    }
  }
  if (part == "Seconds") {
    var.dt = lubridate::second(varx)
    if (all(var.dt == "0")) {
      var.dt = NA
    }
  }
  exp = tibble::add_column(data, name = var.dt)
  names(exp)[length(names(exp))] = name
  return(exp)
}

