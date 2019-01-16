# Convert a variable to dates and times

#' Convert to datetime
#'
#' @param data dataframe
#' @param varname name of the variable
#' @param convname format
#' @param name name of the new column
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
  converted = suppressWarnings(lubridate::parse_date_time(varx, convert))
  exp = tibble::add_column(data, name = converted)
  names(exp)[length(names(exp))] = newname
  return(exp)
}