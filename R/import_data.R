#' Checks if the entered column types are allowed or not.
#' 
#' \code{isColumnTypesCorrect} returns a logical scalar to tell if the 
#' column types entered are valid. Returns TRUE if they are valid. 
#' Returns FALSE if not valid. 
#' 
#' @param col.types a vector of datatypes of columns.
#' @return a logical scalar.
isColumnTypesCorrect <- function(col.types){
  
  allowed.column.types <- c("factor", "numeric", "date")
  
  checked.column.types <- lapply(col.types, function(x){
    
    "%in%"(x, allowed.column.types)
  })
  
  if(FALSE %in% checked.column.types){
    return(FALSE)
  }
  
  else{
    return(TRUE)
  }
}

#' Checks if the complete file was read or not.
#' 
#' \code{isFullFile} returns a logical scalar after checking 
#' the attributes of the dataframe sent as an argument. 
#' Returns TRUE if it is the complete file. Returns FALSE if 
#' it is a partial file read. 
#' 
#' @param user.data.frame the dataframe returned after reading the file
#' @return A logical scalar
isFullFile <- function(user.data.frame){
  
  if (attr(user.data.frame, "full.file") == TRUE){
    return(TRUE)
  }
  
  else{
    return(FALSE)
  }
  
}

#' Converts the data type of columns specified from numeric to date.
#' 
#' \code{convertSpssDetails} converts the specified columns in 
#' the dataframe returned after reading an SPSS file to datatype 
#' Date from numeric.
#' @param user.data.frame the dataframe returned after 
#' reading an spss file.
#' @param date.columns A numeric vector. Has the integer 
#' values of the columns that have to be converted.
#' @return A data frame
convertSpssDetails <- function(user.data.frame, date.columns){
  
  #Convert Dates column in spss.
  date.col2 <- lapply(date.columns, function(x){
    
    user.data.frame[[x]] <- as.Date(user.data.frame[[x]]/86400,
                                    origin = "1582-10-14")
    
  })
  
  user.data.frame[date.columns] <- date.col2
  return(user.data.frame)
}

#' Creates a locale that cane be used with functions from readr package.
#' 
#' \code{makeLocale} returns an object of class locale that has 
#' been created using the arguments passed. The class 
#' locale is defined in the package readr.
#' @param date.names The language used in the file to specift names 
#' of months.
#' @param date.format the format of date in the file.
#' @param time.format the format of time in the file.
#' @param decimal.mark the symbol used as the decimal mark in the file.
#' @param grouping.mark the symbol used as the grouping mark in the file.
#' @param time.zone the timezone used while writing dates/time 
#' in the file.
#' @param encoding.style The encoding style used to make the file.
#' @return Zn object of class locale.
makeLocale <- function(date.names,
                       date.format,
                       time.format,
                       decimal.mark,
                       grouping.mark,
                       time.zone,
                       encoding.style){
  
  user.locale <- readr::locale(date_names    = date.names,
                        date_format   = date.format,
                        time_format   = time.format,
                        decimal_mark  = decimal.mark,
                        grouping_mark = grouping.mark,
                        tz            = time.zone,
                        encoding      = encoding.style)
  
  return(user.locale)
}

##The optional valid arguments for iNZimport() are -
##              1. delim::col.names - TRUE or FALSE
##              2. delim::col.types - One of NULL, a cols specification,
##                                    or a string. Check readr.
##              3. delim::encoding.style = "UTF8" etc
##              4. delim::delim = ',' or "." or "?" etc
##              5. delim::date.names = "en" or "fr" etc
##              6. delim::time.format = "%AT"
##              7. delim::time.zone = ,
##              8. delim::date.format = "%Y-%m-%d" etc
##              9. delim::decimal.mark = "," or "." or "?" etc
##             10. delim::grouping.mark = "," or "." or "?" etc
##             11. excel::sheet = 1 or 2 or 3 etc
##             12. excel::col.names - TRUE or FALSE
##             13. excel::col.types - Either NULL to guess from
##                                    the spreadsheet or a character
##                                    vector containing "blank", "numeric",
##                                    "date" or "text".

#' Reads the file and returns a dataframe according to the arguments
#' passed.
#'
#' \code{iNZread} returns a dataframe by converting the data in the
#' file passed based on the arguments included while passing the file.
#'
#' @title iNZight Import Data
#' @param ... additional arguments
#' @return A dataframe.
#' @import tools
#' @import foreign
#' @import readr
#' @import readxl
#'
#' @author Akshay Gupta
#'
#' @export
iNZread <- function(obj, ...){
  
  UseMethod("iNZread")
  
}

#' @rdname iNZread
#' @param path A string. Specifies the location of the file to be read.
#' @param extension A string. Specifies the extension of the file.
#' @param preview A logical scalar. Should the whole data file be read
#' or only a part of the file be read (100 lines by default where
#' possible.).
#' @param col.types Specifies the class of each column. Null if 
#' not specified.
#' @export
iNZread.default <- function(path, extension = tools::file_ext(path), preview = FALSE, col.types = NULL, ...) {
  
  obj <- structure(list(path = path, preview = preview, col.types = col.types), class = extension)
  
  ## multi methods for some cases
  ## xls, xlsx = excel files
  ## txt, csv  = delim files
  
  class(obj) <- switch(extension,
                       "txt"  = c("txt", "delim"),
                       "xlsx" = c("xslx", "excel"),
                       "xls"  = c("xls", "excel"),
                       "csv"  = c("csv", "delim"))
  
  iNZread(obj, ...)
}

#' @rdname iNZread
#' @export
iNZread.csv <- function(obj, ...){
  
  attr(obj, "delim") = ","
  NextMethod('iNZread', obj)
}

#' @rdname iNZread
#' @export
iNZread.txt <- function(obj, ...){
  
  attr(obj, "delim") = "\t"
  NextMethod('iNZread', obj)
}

#' @rdname iNZread
#' @export
iNZread.xls <- function(obj, ...){
  
  NextMethod('iNZread', obj)
}

#' @rdname iNZread
#' @export
iNZread.xlsx <- function(obj, ...){
  
  NextMethod('iNZread', obj)
}

#' @rdname iNZread
#' @export
iNZread.sav <- function(obj, ...) {
  
  # Factors are retained, levels can be found.
  # Should we include max.value.labels ?
  
  temp.data.frame <- foreign::read.spss(obj$path, ..., to.data.frame = TRUE)
  
  attr(temp.data.frame, "full.file") = TRUE
  
  return(temp.data.frame)
}

#' @rdname iNZread
#' @param number.of.rows number of rows to read
#' @param col.names A logical scalar. Tells if the file 
#' contains column names in the first row or not.
#' @param encoding.style The encoding style used to make the file.
#' @param delim the delimiter used in the file.
#' @param date.names The language used in the file to specift names 
#' of months.
#' @param time.format the format of time in the file.
#' @param time.zone the timezone used while writing dates/time 
#' in the file.
#' @param date.format the format of date in the file.
#' @param decimal.mark the symbol used as the decimal mark in the file.
#' @param grouping.mark the symbol used as the grouping mark in the file.
#' @export
iNZread.delim <- function(obj,
                          ...,
                          number.of.rows = Inf,
                          col.names      = TRUE,
                          encoding.style = "UTF8",
                          delim          = attr(obj, "delim"),
                          date.names     = "en",
                          time.format    = "%AT",
                          time.zone      = Sys.timezone(),
                          date.format    = "%Y-%m-%d",
                          decimal.mark   = (Sys.localeconv())["decimal_point"],
                          grouping.mark  = (Sys.localeconv())["grouping"]) {
  
  new.locale <- makeLocale(date.names,
                           date.format,
                           time.format,
                           decimal.mark,
                           grouping.mark,
                           time.zone,
                           encoding.style)
  
  if (obj$preview == TRUE){
    number.of.rows = 100
  }
  
  temp.data.frame <- readr::read_delim(obj$path,
                                ...,
                                n_max     = number.of.rows,
                                col_names = col.names,
                                col_types = obj$col.types,
                                delim     = delim,
                                locale    = new.locale)
  
  attr(temp.data.frame, "full.file") = !obj$preview
  
  return(temp.data.frame)
}

#' @rdname iNZread
#' @export
iNZread.dta <- function(obj, ...) {
  
  ##Converts stata value labels to create factors. Version 6.0 or later.
  ##Converts dates in stata to dates and POSIX in R.
  
  temp.data.frame <- read.dta(obj$path,
                              ...,
                              convert.dates   = TRUE,
                              convert.factors = TRUE)
  
  attr(temp.data.frame, "full.file") = TRUE
  
  return(temp.data.frame)
}

#' @rdname iNZread
#' @param sheet the number of the sheet which has to be 
#' read from the excel workbook.
#' @param col.names A logical scalar. Tells if the file 
#' contains column names in the first row or not.
#' @export
iNZread.excel <- function(obj,
                          ...,
                          sheet     = 1,
                          col.names = TRUE) {
  
  temp.data.frame <- readxl::read_excel(obj$path,
                                ...,
                                sheet     = sheet,
                                col_names = col.names,
                                col_types = obj$col.types)
  
  attr(temp.data.frame, "full.file") = TRUE
  
  return(temp.data.frame)
}