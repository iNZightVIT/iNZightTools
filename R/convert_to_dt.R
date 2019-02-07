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
# convert_to_datetime = function(data, factorname, convname, newname) {
#   varx = ""
#   for (num in 1:length(factorname)) {
#     name = factorname[num]
#     varx = paste(varx, data[[name]])
#   }
#   order_split = strsplit(convname, " ")
#   convert = ""
#   for (i in order_split) {
#     convert = paste(convert, "%", substring(i, 1, 1), sep = "", collapse = "")
#   }
#   if (convname == "Unix timestamp (secs from 1970)") {
#     varx = as.numeric(varx)
#     converted = as.POSIXct(varx, origin = "1970-01-01")
#   } else {
#     converted <- tryCatch(
#       lubridate::parse_date_time(varx, convert),
#       warning = function(w) if (w$message != "All formats failed to parse. No formats found.") warning(w$message) else return(NA)
#     )
#   }
#   exp = tibble::add_column(data, name = converted)
#   names(exp)[length(names(exp))] = newname
#   return(exp)
# }

## Altered codes
convert_to_datetime <- function(data, factorname, convname, newname) {
  
  mc <- match.call()
  dataname <- mc$.data
  
  ## Subsetting dataset to get varx
  Fname = paste0(".DATA$'", factorname, "'", collapse = ", ")
  Fname = paste0("paste(", Fname,  ")")
  
  ## Working out the convert format
  order_split = strsplit(convname, " ")
  convert.string = ""
  for (i in order_split) {
    convert.string = paste(convert.string, "%", substring(i, 1, 1), sep = "", collapse = "")
  }
  
  ## Actual function
  if (convname == "Unix timestamp (secs from 1970)") {
    Fname = as.numeric(Fname)
    exp <- ~.DATA %>% 
      tibble::add_column(.NAME = as.POSIXct(.VARX, origin = "1970-01-01"), .after = ".AFTER")
  } else {
    exp <- ~.DATA %>% 
      tibble::add_column(.NAME = lubridate::parse_date_time(.VARX, convert), .after = ".AFTER")
  }
  
  ## Replacing variables
  exp <- iNZightTools:::replaceVars(exp, 
                                    .NAME = newname,
                                    .VARX = Fname,
                                    .DATA = data,
                                    .VARNAME = factorname,
                                    .AFTER = factorname[length(factorname)]
  )
  
  interpolate(exp, convert = convert.string)
}