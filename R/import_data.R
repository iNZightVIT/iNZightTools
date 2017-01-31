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
#' \code{isPreview} returns a logical scalar after checking 
#' the attributes of the dataframe sent as an argument. 
#' Returns TRUE if it was read as a preview. 
#' 
#' @param user.data.frame the dataframe returned after reading the file
#' @return A logical scalar
isPreview <- function(user.data.frame){
  
  if("preview" %in% attributes(user.data.frame)){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
  
}

##Don't give options that we can't handle. 
##Referring to rightclick>options for converting data to.

#' Changes the datatype of columns of dataframe to the specified datatypes. 
#' 
#' \code{changeColumnTypes} returns a data frame with changed datatypes of columns. 
#' Returns the same data frame if column types are specified as NULL.
#' 
#' @param user.data.frame dataframe which needs the datatypes of columns changed.
#' @param col.types a vector of datatypes of columns.
#' @return a data frame.
changeColumnTypes <- function(user.data.frame, col.types){
  
  check.result <- lapply(seq_along(names(user.data.frame)), function(x){
    
    if (class(user.data.frame[[x]]) == col.types[[x]]){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
    
  })
  
  if (!all(check.result)) {
    temp.return <- lapply(seq_along(names(user.data.frame)), function(x){
      if(!check.result[[x]]){
        changeToDatatype(user.data.frame, x, col.types)
      }
      else{
        user.data.frame[x]
      }
    })
    
    #print(temp.return)
    user.data.frame <- do.call("cbind", lapply(temp.return, data.frame))
    ## convert incorrect columns only
  }
  
  return(user.data.frame)
}

#' Changes the datatype of a column of a dataframe to the specified datatype. 
#' 
#' \code{changeToDatatype} returns a data frame with 1 column 
#' (just the column whose data type has been changed) with changed 
#' datatypes of column. 
#' 
#' @param ... Other arguments.
#' 
#' @return a data frame.
changeToDatatype <- function(obj, ...){
  
  UseMethod("changeToDatatype")
}

#' @rdname changeToDatatype
#' @param user.data.frame dataframe which needs the datatypes of columns changed.
#' @param x column number of the column which needs the data type changed.
#' @param col.types a vector of datatypes of columns.
changeToDatatype.default <- function(user.data.frame, x = 0, col.types = NULL){
  
  obj <- "Column Change"
  attr(obj, "user.data.frame") <- user.data.frame
  attr(obj, "column.number") <- x
  class(obj) <- paste("iNZclass", col.types[x], sep = '.')

  changeToDatatype(obj)
  
}

#' @rdname changeToDatatype
changeToDatatype.iNZclass.none <- function(obj){
  
  user.data.frame <- attr(obj, "user.data.frame")
  column.number <- attr(obj, "column.number")
  
  return(user.data.frame[column.number])
}

#' @rdname changeToDatatype
changeToDatatype.iNZclass.numeric <- function(obj){
  
  #print("Entered numeric")
  user.data.frame <- attr(obj, "user.data.frame")
  column.number <- attr(obj, "column.number")
  
  user.data.frame[[column.number]] <- as.numeric(user.data.frame[[column.number]])
  #print(head(user.data.frame))
  return(user.data.frame[column.number])
}

#' @rdname changeToDatatype
changeToDatatype.iNZclass.character <- function(obj){
  
  #print("Entered character")
  user.data.frame <- attr(obj, "user.data.frame")
  column.number <- attr(obj, "column.number")
  
  user.data.frame[[column.number]] <- as.character(user.data.frame[[column.number]])
  return(user.data.frame[column.number])
}

#' @rdname changeToDatatype
changeToDatatype.iNZclass.factor <- function(obj){
  
  user.data.frame <- attr(obj, "user.data.frame")
  column.number <- attr(obj, "column.number")
  
  user.data.frame[[column.number]] <- as.factor(user.data.frame[[column.number]])
  
  if ("metadata" %in% names(attributes(user.data.frame))){
    
    if (colnames(user.data.frame[column.number]) %in% 
        attr(user.data.frame, "metadata")){
      
      if (attr(test.meta[test.meta == "height"][[1]], "datatype") == "factor"){
        
        ##Set levels and do other stuff. 
      }
    }
  }

  return(user.data.frame[column.number])
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

#' Reads the file and returns a dataframe according to the arguments
#' passed.
#'
#' \code{iNZread} returns a dataframe by converting the data in the
#' file passed based on the arguments included while passing the file.
#'
#' @title iNZight Import Data
#' @param path A string. Specifies the location of the file to be read.
#' @param col.types A character vector specifying the datatypes of columns.
#' @param ... additional arguments
#' @return A dataframe.
#'
#' @author Akshay Gupta
#'
#' @export
iNZread <- function(path, col.types = NULL, ...) {
  user.data.frame <- .iNZread(path = path, col.types = col.types, ...)
  
  #print(attributes(user.data.frame))
  #print("metadata" %in% names(attributes(user.data.frame)))
  
    
  if(is.null(col.types)){
    
    print("Null col.types checked")
    
    if ("metadata" %in% names(attributes(user.data.frame))){
      
      #print("entered checking of metadata and it is available.")
      metadata.list <- attr(user.data.frame, "metadata")  
      #print(metadata.list)
      
      columns.types.list <- lapply(colnames(user.data.frame), function(x){
        
        if (x %in% metadata.list){
          
          attr(metadata.list[metadata.list == x][[1]], "datatype")
        }
        else{
          
          "none"
        }
      })
      
      col.types <- sapply(columns.types.list, paste, collapse = ",")
    }
    else{
      
      return(user.data.frame)
    }
  }
  
  user.data.frame <- changeColumnTypes(user.data.frame, col.types)
  return(user.data.frame)
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


#' \code{.iNZread} returns a dataframe by converting the data in the
#' file passed based on the arguments included while passing the file.
#' @param path A string. Specifies the location of the file to be read.
#' @param ... additional arguments
#' @return A dataframe.
#'
#' @export
.iNZread <- function(path, col.types, ...){
  
  UseMethod(".iNZread")
  
}

#' @rdname .iNZread
#' @param path A string. Specifies the location of the file to be read.
#' @param extension A string. Specifies the extension of the file.
#' @param preview A logical scalar. Should the whole data file be read
#' or only a part of the file be read (100 lines by default where
#' possible.).
#' @param col.types Specifies the class of each column. Null if 
#' not specified.
#' @export
.iNZread.default <- function(path, extension = tools::file_ext(path), preview = FALSE, col.types, ...) {
  
  #obj <- structure(list(path = path, preview = preview, col.types = col.types), class = extension)
  
  ## multi methods for some cases
  ## xls, xlsx = excel files
  ## txt, csv  = delim files
  #print(col.types)
  
  class(path) <- switch(extension[1],
                       "txt"  = c("txt", "delim"),
                       "xlsx" = c("xslx", "excel"),
                       "xls"  = c("xls", "excel"),
                       "csv"  = c("csv", "delim"),
                       extension)
  
  .iNZread(path, preview = preview, col.types = col.types, ...)
}

#' @rdname .iNZread
#' @export
.iNZread.csv <- function(path, ...){
  
  attr(path, "delim") = ","
  NextMethod('.iNZread', path)
}

#' @rdname .iNZread
#' @export
.iNZread.txt <- function(path, ...){
  
  attr(path, "delim") = "\t"
  NextMethod('.iNZread', path)
}

#' @rdname .iNZread
#' @export
.iNZread.xls <- function(path, ...){
  
  NextMethod('.iNZread', path)
}

#' @rdname .iNZread
#' @export
.iNZread.xlsx <- function(path, ...){
  
  NextMethod('.iNZread', path)
}

#' @rdname .iNZread
#' @export
.iNZread.sav <- function(path, ...) {
  
  # Factors are retained, levels can be found.
  # Should we include max.value.labels ?
  
  temp.data.frame <- foreign::read.spss(path, to.data.frame = TRUE)
  
  #attr(temp.data.frame, "preview") = obj$preview
  
  return(temp.data.frame)
}

#' @rdname .iNZread
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
.iNZread.delim <- function(path, 
                           col.types,
                           preview,
                           comment = "#",
                           number.of.rows = Inf,
                           col.names      = TRUE,
                           encoding.style = "UTF8",
                           delim          = attr(path, "delim"),
                           date.names     = "en",
                           time.format    = "%AT",
                           time.zone      = Sys.timezone(),
                           date.format    = "%Y-%m-%d",
                           decimal.mark   = (Sys.localeconv())["decimal_point"],
                           grouping.mark  = (Sys.localeconv())["grouping"],
                           ...) {
  
  ##Source the file which contains code for evaluating metadata. 
  source("read_metadataV4.R")
  
  ##Source the file which contains code to convert types to char.
  source("column_types_to_char.R")
  
  new.locale <- makeLocale(date.names,
                           date.format,
                           time.format,
                           decimal.mark,
                           grouping.mark,
                           time.zone,
                           encoding.style)
  
  if (preview == TRUE){
    number.of.rows = 100
  }
  
  metadata.available <- isMetadataAvailable(path)
  
  if(metadata.available){
    
    metadata.list <- readMetadata(path = path)
  }
  
  if (!is.null(col.types)){
    
    col.types <- makeDatatypeChar(col.types)
  }
  else{
    
    if(metadata.available){
      
      column.names.line <- findColumnNames(path)
      
      ##Check if metadata(column.names) match returned column names.
      check.column.names <- checkForColumnNames(metadata.list, column.names.line)

      if(check.column.names){
        
        ##Get a vector of column names from columns.names.line
        column.names.list <- strsplit(column.names.line, split = ",")
        
        ##Use metadata to form the column types and attach metadatalist to the userdataframe.
        col.types <- makeDatatypeChar(metadata.list, column.names.list[[1]])
      }
    }
  }
    
  temp.data.frame <- readr::read_delim(path,
                                       col_types = col.types,
                                       comment   = comment,
                                       n_max     = number.of.rows,
                                       col_names = col.names,
                                       delim     = delim,
                                       locale    = new.locale)
  
  if (metadata.available){
    
    attr(temp.data.frame, "metadata") <- metadata.list
  }
  
  if (preview){
    attr(temp.data.frame, "preview") = 1
  }
  
  return(temp.data.frame)
}

#' @rdname .iNZread
#' @export
.iNZread.dta <- function(path, ...) {
  
  ##Converts stata value labels to create factors. Version 6.0 or later.
  ##Converts dates in stata to dates and POSIX in R.
  
  temp.data.frame <- foreign::read.dta(path,
                                       convert.dates   = TRUE,
                                       convert.factors = TRUE)
  
  #attr(temp.data.frame, "preview") = obj$preview
  
  return(temp.data.frame)
}

#' @rdname .iNZread
#' @param sheet the number of the sheet which has to be 
#' read from the excel workbook.
#' @param col.names A logical scalar. Tells if the file 
#' contains column names in the first row or not.
#' @export
.iNZread.excel <- function(path,
                           col.types,
                           sheet     = 1,
                           col.names = TRUE,
                           ...) {
  
  temp.data.frame <- readxl::read_excel(path,
                                        sheet     = sheet,
                                        col_names = col.names,
                                        col_types = col.types)
  
  #attr(temp.data.frame, "preview") = obj$preview
  
  return(temp.data.frame)
}
