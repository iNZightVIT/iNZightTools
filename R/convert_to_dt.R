# Convert a variable to dates and times

#' Convert to datetime
#'
#' @param data dataframe
#' @param factorname name of the variable
#' @param convname format
#' @param newname name of the new column
#'
#' @return dataframe with datatime column
#' @export
#' @author Yiwen He
convert_to_datetime = function(data, factorname, convname, newname) {
  varx = ""
  for (num in 1:length(factorname)) {
    name = factorname[num]
    varx = paste(varx, data[[name]])
  }
  order_split = strsplit(convname, " ")
  convert = ""
  for (i in order_split) {
    convert = paste(convert, "%", substring(i, 1, 1), sep = "", collapse = "")
  }
  if (convname == "Unix timestamp (secs from 1970)") {
    varx = as.numeric(varx)
    converted = as.POSIXct(varx, origin = "1970-01-01")
  } else {
    converted <- tryCatch(
      lubridate::parse_date_time(varx, convert),
      warning = function(w) if (w$message != "All formats failed to parse. No formats found.") warning(w$message) else return(NA)
    )
  }
  exp = tibble::add_column(data, name = converted)
  names(exp)[length(names(exp))] = newname
  return(exp)
}


xxt <- tryCatch(
  lubridate::parse_date_time("this is not a date", "ymd"),
  warning = function(w) if (w$message != "All formats failed to parse. No formats found.") warning(w$message) else return(NA)
)
