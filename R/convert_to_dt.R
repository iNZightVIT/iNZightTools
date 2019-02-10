# Convert a variable to dates and times

#' Convert to datetime
#'
#' @param .data dataframe
#' @param factorname name of the variable
#' @param convname format
#' @param newname name of the new column
#'
#' @return dataframe with datatime column
#' @export
#' @author Yiwen He

# Something to think about
#     converted <- tryCatch(
#       lubridate::parse_date_time(varx, convert),
#       warning = function(w) if (w$message != "All formats failed to parse. No formats found.") warning(w$message) else return(NA)

convert_to_datetime <- function(.data, factorname, convname, newname) {
  
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
  if (convert.string == "%U%t%(%f%1") {
    Fname = paste0("as.numeric(", Fname, ")")
    exp <- ~.DATA %>% 
      tibble::add_column(.NAME = as.POSIXct(.VARX, origin = "1970-01-01"), .after = ".AFTER")
  } else {
    exp <- ~.DATA %>% 
      tibble::add_column(.NAME = lubridate::parse_date_time(.VARX, convert), .after = ".AFTER")
  }

  ## Replacing variables
  exp <- replaceVars(exp, 
                    .NAME = newname,
                    .VARX = Fname,
                    .DATA = dataname,
                    .VARNAME = factorname,
                    .AFTER = factorname[length(factorname)]
                    )
  
  interpolate(exp, convert = convert.string)
}

