

convertSpssDetails <- function(user.data.frame, date.columns){

  #Convert Dates column in spss.
  date.col2 <- lapply(date.columns, function(x){

    user.data.frame[[x]] <- as.Date(user.data.frame[[x]]/86400,
                                    origin = "1582-10-14")

  })

  user.data.frame[date.columns] <- date.col2
  return(user.data.frame)
}

makeLocale <- function(date.names,
                       date.format,
                       time.format,
                       decimal.mark,
                       grouping.mark,
                       time.zone,
                       encoding.style){

  user.locale <- locale(date_names    = date.names,
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
#' @return A dataframe of length 100 or less if the file extension
#' is "csv" or "txt" (if partial file reading is possible) or a
#' dataframe of length equal to the file.
#'
#' @import tools
#' @import foreign
#' @import readr
#' @import readxl
#'
#' @author Akshay Gupta
#'
#' @examples
#' iNZread("\user\Akshay\datafile.txt")
#' iNZread("\user\Akshay\datafile.txt", "csv")
#' iNZread("\user\Akshay\datafile.csv", "csv", TRUE)
#' iNZread("\user\Akshay\datafile.csv", col.names = FALSE)
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
#' @export
iNZread.default <- function(path, extension = tools::file_ext(path), preview = FALSE, ...) {

  obj <- structure(list(path = path, preview = preview), class = extension)

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

#' @export
iNZread.csv <- function(obj, ...){

  attr(obj, "delim") = ","
  NextMethod('iNZread', obj)
}

#' @export
iNZread.txt <- function(obj, ...){

  attr(obj, "delim") = "\t"
  NextMethod('iNZread', obj)
}

#' @export
iNZread.xls <- function(obj, ...){

  NextMethod('iNZread', obj)
}

#' @export
iNZread.xlsx <- function(obj, ...){

  NextMethod('iNZread', obj)
}

#' @export
iNZread.sav <- function(obj, ...) {

  # Factors are retained, levels can be found.
  # Should we include max.value.labels ?

  temp.data.frame <- read.spss(obj$path, ..., to.data.frame = TRUE)

  attr(temp.data.frame, "full.file") = TRUE

  return(temp.data.frame)
}

#' @rdname iNZread
#' @param number.of.rows number of rows to read
#' @export
iNZread.delim <- function(obj,
                          ...,
                          number.of.rows = Inf,
                          col.names      = TRUE,
                          col.types      = NULL,
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

  temp.data.frame <- read_delim(obj$path,
                                ...,
                                n_max     = number.of.rows,
                                col_names = col.names,
                                col_types = col.types,
                                delim     = delim,
                                locale    = new.locale)

  attr(temp.data.frame, "full.file") = !obj$preview

  return(temp.data.frame)
}

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

iNZread.excel <- function(obj,
                          ...,
                          sheet     = 1,
                          col.names = TRUE,
                          col.types = NULL) {

  temp.data.frame <- read_excel(obj$path,
                                ...,
                                sheet     = sheet,
                                col_names = col.names,
                                col_types = col.types)

  attr(temp.data.frame, "full.file") = TRUE

  return(temp.data.frame)
}
