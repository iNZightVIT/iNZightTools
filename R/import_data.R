##' A simple function that magically imports a file, irrespective of type.
##'
##' @title iNZight Smart Read
##' @param file the file path to read
##' @param ext file extension, namely "csv" or "txt"
##' @param preview logical, if \code{TRUE} only the first few rows of
##'   the data will be returned
##' @param column_types vector of column types (see \code{?readr::read_csv})
##' @param ... additional parameters passed to read_* functions
##' @return a dataframe with attributes
##' @author Tom Elliott
##' @export
smart_read <- function(file, ext = tools::file_ext(file), preview = FALSE, column_types, ...) {
    type <- guess_type(ext)
    fun <- eval(parse(text = sprintf("read_%s", type)))
    d <- fun(file, ext = ext, preview = preview, column_types = column_types, ...)
    if (preview) 
      class(d) <- c('inz.preview', class(d))
    if (is.null(attr(d, "name")))
      attr(d, "name") <- tools::file_path_sans_ext(basename(file))
    d
}

guess_type <- function(ext) {
    switch(ext,
           "xls" = "excel",
           "xlsx" = "excel",
           "sav" = "spss",
           "dta" = "stata",
           "sas7bdat" = "sas",
           "txt" = "meta",
           "csv" = "meta", ## -> metadata_read.R
           "unknown")
}

read_unknown <- function(file, ...) {
  warning("Unable to read file: ", file)
  return(NULL)
}

read_dlm <- function(file, ext = tools::file_ext(file), preview = FALSE, column_types,
                     encoding, delimiter, decimal_mark, grouping_mark,
                     convert.to.factor = TRUE,
                     ...) {

    named.args <- list(...)
    
    if (is.null(named.args$comment))
        named.args$comment <- "#"

    if (preview)
        named.args <- c(list(n_max = 10), named.args)

    if (missing(delimiter))
        delimiter <- ifelse(ext == "csv", ",", " ")
    
    if (ext != "csv" || delimiter != ",")
        named.args <- c(list(delim = delimiter), named.args)
    else if (ext == "txt")
        named.args <- c(list(delim = " "), named.args)

    locale <- list()
    if (!missing(encoding))
        locale$encoding <- escape_string(encoding)

    if (!missing(decimal_mark))
        locale$decimal_mark <- escape_string(decimal_mark)
    
    if (!missing(grouping_mark))
        locale$grouping_mark <- escape_string(grouping_mark)

    ## quote character arguments (x = z -> x = "z")
    named.args <- lapply(named.args,
                         function(x) {
                             if (is.character(x)) escape_string(x)
                             else x
                         })
    
    ctypes <- ""
    if (!missing(column_types)) {
        named.args <- c(list(col_types = "COLTYPES"))

        if (!is.null(names(column_types))) {
            ctypes <- paste("readr::cols(", names(column_types), " = '", column_types, "')",
                            sep = "", collapse = ", ")
        } else {
            ctypes <- paste("readr::cols('", column_types, "')", sep = "", collapse = "")
        }
    }

    if (length(locale) > 0) 
        named.args$locale <- sprintf("readr::locale(%s)",
                                     paste(names(locale), locale,
                                           sep = " = ", collapse = ", "))
    
    if (length(named.args) > 0)
        args <- paste("file,",
                      paste(names(named.args), named.args,
                            collapse = ", ", sep = " = "))
    else
        args <- "file"
    
    exp <- ~FUN(ARGS)
    exp <- replaceVars(exp,
                       FUN = sprintf("readr::read_%s",
                                     ifelse(ext == "csv" && delimiter == ",", 
                                            "csv", "delim")),
                       ARGS = args,
                       COLTYPES = ctypes)

    TEMP_RESULT <- interpolate(exp, file = file)
    if (!convert.to.factor) return(TEMP_RESULT)
    
    chars <- sapply(TEMP_RESULT, is.character)
    if (!any(chars)) return(TEMP_RESULT)

    ## mutate(name = factor(name))
    charnames <- names(TEMP_RESULT)[chars]
    expr2 <- paste(
        "TEMP_RESULT %>% dplyr::mutate(",
        paste("\"", charnames, "\" = as.factor(", 
              quote_varname(charnames), 
              ")",
              sep = "", collapse = ", "),
        ")", sep = "")

    res2 <- eval(parse(text = expr2))
    attr(res2, "code") <-
        gsub("TEMP_RESULT", paste(code(TEMP_RESULT), collapse="\n"), expr2)
    res2
}

read_excel <- function(file, ext, preview = FALSE, column_types, ...) {
    named.args <- list(...)

    if (!missing(column_types))
        named.args <- c(list(col_types = column_types), named.args)
        
    if (preview)
        named.args <- c(list(n_max = 10), named.args)

    if (length(named.args) > 0)
        args <- paste("file,",
                      paste(names(named.args), named.args,
                            collapse = ", ", sep = " = "))
    else
        args <- "file"
    
    exp <- ~readxl::read_excel(ARGS)
    exp <- replaceVars(exp, ARGS = args)

    interpolate(exp, file = file)
}

read_spss <- function(file, ext, preview = FALSE, column_types) {
    exp <- ~foreign::read.spss(file, to.data.frame = TRUE)
    
    interpolate(exp, file = file)
}

read_stata <- function(file, ext, preview = FALSE, column_types) {
    exp <- ~foreign::read.dta(file)
    interpolate(exp, file = file)
}

escape_string <- function(x) sprintf("\"%s\"", x)

quote_varname <- function(x, q = "`") {
    ## contains any non alphanumeric characters, OR first character is number
    xs <- grepl("[^a-zA-Z0-9]", x) | grepl("^[0-9]", x)
    if (any(xs)) {
        x[xs] <- paste0(q, x[xs], q)
    }
    x
}

#' Checks if the complete file was read or not.
#'
#' @title Is Preview
#' @param df data to check
#' @return logical
#' @export
is_preview <- function(df) inherits(df, "inz.preview")


# #' Checks if the entered column types are allowed or not.
# #'
# #' \code{isColumnTypesCorrect} returns a logical scalar to tell if the
# #' column types entered are valid. Returns TRUE if they are valid.
# #' Returns FALSE if not valid.
# #'
# #' @param col.types a vector of datatypes of columns.
# #' @return a logical scalar.
# isColumnTypesCorrect <- function(col.types){

#   allowed.column.types <- c("factor", "numeric", "date")

#   checked.column.types <- lapply(col.types, function(x){

#     "%in%"(x, allowed.column.types)
#   })

#   if(FALSE %in% checked.column.types){
#     return(FALSE)
#   }

#   else{
#     return(TRUE)
#   }
# }


# #' Changes the datatype of columns of dataframe to the specified datatypes.
# #'
# #' \code{changeColumnTypes} returns a data frame with changed datatypes of columns.
# #' Returns the same data frame if column types are specified as NULL.
# #'
# #' @param user.data.frame dataframe which needs the datatypes of columns changed.
# #' @param col.types a vector of datatypes of columns.
# #' @return a data frame.
# changeColumnTypes <- function(user.data.frame, col.types){

#   check.result <- sapply(seq_along(names(user.data.frame)), function(x){

#     if (class(user.data.frame[[x]]) == col.types[[x]]){
#       return(TRUE)
#     }
#     else{
#       return(FALSE)
#     }

#   })

#   if (!all(check.result)) {
#     temp.return <- lapply(seq_along(names(user.data.frame)), function(x){
#       if(!check.result[[x]]){
#         changeToDatatype(user.data.frame, x, col.types)
#       }
#       else{
#         user.data.frame[x]
#       }
#     })

#     user.data.frame <- do.call("cbind", lapply(temp.return, data.frame))
#   }

#   return(user.data.frame)
# }

# # changeToDatatype <- function(obj, ...){
# #
# #   UseMethod("changeToDatatype")
# # }


# #' Changes the datatype of a column of a dataframe to the specified datatype.
# #'
# #' \code{changeToDatatype} returns a data frame with 1 column
# #' (just the column whose data type has been changed) with changed
# #' datatypes of column.
# #'
# #' @param ... Other arguments.
# #'
# #' @return a data frame.
# #' @rdname changeToDatatype
# #' @param user.data.frame dataframe which needs the datatypes of columns changed.
# #' @param x column number of the column which needs the data type changed.
# #' @param col.types a vector of datatypes of columns.
# changeToDatatype <- function(user.data.frame, x = 0, col.types = NULL){

#   obj <- "Column Change"
#   attr(obj, "user.data.frame") <- user.data.frame
#   attr(obj, "column.number") <- x
#   class(obj) <- paste("iNZclass", col.types[x], sep = '.')

#   .changeToDatatype(obj)

# }

# .changeToDatatype <- function(obj) UseMethod(".changeToDatatype")

# # #' @rdname changeToDatatype
# .changeToDatatype.default <- function(obj){

#   user.data.frame <- attr(obj, "user.data.frame")
#   column.number <- attr(obj, "column.number")

#   return(user.data.frame[column.number])
# }

# # #' @rdname changeToDatatype
# .changeToDatatype.iNZclass.numeric <- function(obj){

#   user.data.frame <- attr(obj, "user.data.frame")
#   column.number <- attr(obj, "column.number")

#   user.data.frame[[column.number]] <- as.numeric(user.data.frame[[column.number]])
#   return(user.data.frame[column.number])
# }

# # #' @rdname changeToDatatype
# .changeToDatatype.iNZclass.character <- function(obj){

#   user.data.frame <- attr(obj, "user.data.frame")
#   column.number <- attr(obj, "column.number")

#   user.data.frame[[column.number]] <- as.character(user.data.frame[[column.number]])
#   return(user.data.frame[column.number])
# }

# # #' @rdname changeToDatatype
# .changeToDatatype.iNZclass.factor <- function(obj){

#   user.data.frame <- attr(obj, "user.data.frame")
#   column.number <- attr(obj, "column.number")

#   if ("metadata" %in% names(attributes(user.data.frame))){

#     metadata.list <- attr(user.data.frame, "metadata")
#     column.name <- colnames(user.data.frame)[column.number]

#     if (column.name %in% metadata.list){

#       datatype.specified.in.metadata <- attr(metadata.list[metadata.list ==
#                                                              column.name][[1]], "datatype")

#       if (datatype.specified.in.metadata == "factor"){

#         if("values" %in% names(attributes(metadata.list[metadata.list == column.name][[1]]))){

#           ##Set levels and do other stuff.
#           column.values.from.metadata <- attr(metadata.list[metadata.list == column.name][[1]], "values")[[1]]

#           #source("read_metadata.R")
#           column.values.and.labels <- convertToLevelsAndLabels(column.values.from.metadata)
#           labels.vector <- column.values.and.labels[["labels.vector"]]
#           levels.vector <- column.values.and.labels[["levels.vector"]]

#           if(length(labels.vector) == length(levels.vector)){

#             user.data.frame[[column.number]] <- factor(user.data.frame[[column.number]], levels = levels.vector, labels = labels.vector)
#           }
#           else{
#             user.data.frame[[column.number]] <- factor(user.data.frame[[column.number]], levels = labels.vector )
#           }
#         }
#         else{

#           user.data.frame[[column.number]] <- as.factor(user.data.frame[[column.number]])
#         }
#       }
#       else{

#         user.data.frame[[column.number]] <- as.factor(user.data.frame[[column.number]])
#       }
#     }
#     else{

#       user.data.frame[[column.number]] <- as.factor(user.data.frame[[column.number]])
#     }
#   }
#   else{

#     user.data.frame[[column.number]] <- as.factor(user.data.frame[[column.number]])
#   }

#   return(user.data.frame[column.number])
# }

# #' Converts the data type of columns specified from numeric to date.
# #'
# #' \code{convertSpssDetails} converts the specified columns in
# #' the dataframe returned after reading an SPSS file to datatype
# #' Date from numeric.
# #' @param user.data.frame the dataframe returned after
# #' reading an spss file.
# #' @param date.columns A numeric vector. Has the integer
# #' values of the columns that have to be converted.
# #' @return A data frame
# convertSpssDetails <- function(user.data.frame, date.columns){

#   #Convert Dates column in spss.
#   date.col2 <- lapply(date.columns, function(x){

#     user.data.frame[[x]] <- as.Date(user.data.frame[[x]]/86400,
#                                     origin = "1582-10-14")

#   })

#   user.data.frame[date.columns] <- date.col2
#   return(user.data.frame)
# }

# #' Creates a locale that cane be used with functions from readr package.
# #'
# #' \code{makeLocale} returns an object of class locale that has
# #' been created using the arguments passed. The class
# #' locale is defined in the package readr.
# #' @param date.names The language used in the file to specift names
# #' of months.
# #' @param date.format the format of date in the file.
# #' @param time.format the format of time in the file.
# #' @param decimal.mark the symbol used as the decimal mark in the file.
# #' @param grouping.mark the symbol used as the grouping mark in the file.
# #' @param time.zone the timezone used while writing dates/time
# #' in the file.
# #' @param encoding.style The encoding style used to make the file.
# #' @return Zn object of class locale.
# makeLocale <- function(date.names,
#                        date.format,
#                        time.format,
#                        decimal.mark,
#                        grouping.mark,
#                        time.zone,
#                        encoding.style){

#   user.locale <- readr::locale(date_names    = date.names,
#                         date_format   = date.format,
#                         time_format   = time.format,
#                         decimal_mark  = decimal.mark,
#                         grouping_mark = grouping.mark,
#                         tz            = time.zone,
#                         encoding      = encoding.style)

#   return(user.locale)
# }

# #' Reads the file and returns a dataframe according to the arguments
# #' passed.
# #'
# #' \code{iNZread} returns a dataframe by converting the data in the
# #' file passed based on the arguments included while passing the file.
# #'
# #' @title iNZight Import Data
# #' @param path A string. Specifies the location of the file to be read.
# #' @param col.types A character vector specifying the datatypes of columns.
# #' @param ... additional arguments
# #' @return A dataframe.
# #'
# #' @author Akshay Gupta
# #'
# #' @export
# iNZread <- function(path, col.types = NULL, ...) {
#   user.data.frame <- .iNZread(path = path, col.types = col.types, ...)

#   if(is.null(col.types)){

#     if ("metadata" %in% names(attributes(user.data.frame))){

#       metadata.list <- attr(user.data.frame, "metadata")
#       columns.types.list <- lapply(colnames(user.data.frame), function(x){

#         if (x %in% metadata.list){

#           attr(metadata.list[metadata.list == x][[1]], "datatype")
#         }
#         else{

#           "none"
#         }
#       })

#       col.types <- sapply(columns.types.list, paste, collapse = ",")
#     }
#     else{

#       return(user.data.frame)
#     }
#   }

#   newdf <- changeColumnTypes(user.data.frame, col.types)
#   if (isPreview(user.data.frame))
#       attr(newdf, "preview") <- attr(user.data.frame, "preview")

#   newdf
# }


# ##The optional valid arguments for iNZimport() are -
# ##              1. delim::col.names - TRUE or FALSE
# ##              2. delim::col.types - One of NULL, a cols specification,
# ##                                    or a string. Check readr.
# ##              3. delim::encoding.style = "UTF8" etc
# ##              4. delim::delim = ',' or "." or "?" etc
# ##              5. delim::date.names = "en" or "fr" etc
# ##              6. delim::time.format = "%AT"
# ##              7. delim::time.zone = ,
# ##              8. delim::date.format = "%Y-%m-%d" etc
# ##              9. delim::decimal.mark = "," or "." or "?" etc
# ##             10. delim::grouping.mark = "," or "." or "?" etc
# ##             11. excel::sheet = 1 or 2 or 3 etc
# ##             12. excel::col.names - TRUE or FALSE
# ##             13. excel::col.types - Either NULL to guess from
# ##                                    the spreadsheet or a character
# ##                                    vector containing "blank", "numeric",
# ##                                    "date" or "text".

# .iNZread <- function(path, col.types, ...){

#   UseMethod(".iNZread")

# }

# # #' @rdname .iNZread
# # #' @param path A string. Specifies the location of the file to be read.
# # #' @param extension A string. Specifies the extension of the file.
# # #' @param preview A logical scalar. Should the whole data file be read
# # #' or only a part of the file be read (100 lines by default where
# # #' possible.).
# # #' @param col.types Specifies the class of each column. Null if
# # #' not specified.
# # #' @export
# .iNZread.default <- function(path, extension = tools::file_ext(path), preview = FALSE, col.types, ...) {

#   class(path) <- switch(extension[1],
#                        "txt"  = c("txt", "delim"),
#                        "xlsx" = c("xslx", "excel"),
#                        "xls"  = c("xls", "excel"),
#                        "csv"  = c("csv", "delim"),
#                        extension)

#   .iNZread(path, preview = preview, col.types = col.types, ...)
# }

# # #' @rdname .iNZread
# # #' @export
# .iNZread.csv <- function(path, ...){

#   attr(path, "delim") = ","
#   NextMethod('.iNZread', path)
# }

# # #' @rdname .iNZread
# # #' @export
# .iNZread.txt <- function(path, ...){

#   attr(path, "delim") = "\t"
#   NextMethod('.iNZread', path)
# }

# # #' @rdname .iNZread
# # #' @export
# .iNZread.xls <- function(path, ...){

#   NextMethod('.iNZread', path)
# }

# # #' @rdname .iNZread
# # #' @export
# .iNZread.xlsx <- function(path, ...){

#   NextMethod('.iNZread', path)
# }

# # #' @rdname .iNZread
# # #' @export
# .iNZread.sav <- function(path, ...) {

#   temp.data.frame <- foreign::read.spss(path, to.data.frame = TRUE)
#   return(temp.data.frame)
# }

# # #' @rdname .iNZread
# # #' @param number.of.rows number of rows to read
# # #' @param col.names A logical scalar. Tells if the file
# # #' contains column names in the first row or not.
# # #' @param encoding.style The encoding style used to make the file.
# # #' @param delim the delimiter used in the file.
# # #' @param date.names The language used in the file to specift names
# # #' of months.
# # #' @param time.format the format of time in the file.
# # #' @param time.zone the timezone used while writing dates/time
# # #' in the file
# # #' @param date.format the format of date in the file.
# # #' @param decimal.mark the symbol used as the decimal mark in the file.
# # #' @param grouping.mark the symbol used as the grouping mark in the file.
# # #' @export
# .iNZread.delim <- function(path,
#                            col.types,
#                            preview,
#                            comment = "#",
#                            number.of.rows = Inf,
#                            col.names      = TRUE,
#                            encoding.style = "UTF-8",
#                            delim          = attr(path, "delim"),
#                            date.names     = "en",
#                            time.format    = "%AT",
#                            time.zone      = 
#                               ifelse(is.na(Sys.timezone()), 'NZ', Sys.timezone()),
#                            date.format    = "%Y-%m-%d",
#                            decimal.mark   = (Sys.localeconv())["decimal_point"],
#                            grouping.mark  = (Sys.localeconv())["grouping"],
#                            ...) {

#   new.locale <- makeLocale(date.names,
#                            date.format,
#                            time.format,
#                            decimal.mark,
#                            grouping.mark,
#                            time.zone,
#                            encoding.style)

#   if (preview == TRUE){
#     number.of.rows = 20
#   }

#   metadata.available <- isMetadataAvailable(path)

#   if(metadata.available){

#     metadata.list <- readMetadata(path = path)
#   }

#   if (!is.null(col.types)){
#     col.types <- makeDatatypeChar(col.types)
#   }
#   else{

#     if(metadata.available){

#       column.names.line <- findColumnNames(path)

#       ##Check if metadata(column.names) match returned column names.
#       check.column.names <- checkForColumnNames(metadata.list, column.names.line)

#       if(check.column.names){

#         ##Get a vector of column names from columns.names.line
#         column.names.list <- strsplit(column.names.line, split = ",")

#         ##Use metadata to form the column types and attach metadatalist to the userdataframe.
#         col.types <- makeDatatypeChar(metadata.list, column.names.list[[1]])
#       }
#     }
#   }

#   temp.data.frame <- as.data.frame(readr::read_delim(path,
#                                        col_types = col.types,
#                                        comment   = comment,
#                                        n_max     = number.of.rows,
#                                        col_names = col.names,
#                                        delim     = delim,
#                                        locale    = new.locale))

#   if (metadata.available){

#     attr(temp.data.frame, "metadata") <- metadata.list
#   }

#   if (preview){
#     attr(temp.data.frame, "preview") = 1
#   }

#   return(temp.data.frame)
# }

# # #' @rdname .iNZread
# # #' @export
# .iNZread.dta <- function(path, ...) {

#   ##Converts stata value labels to create factors. Version 6.0 or later.
#   ##Converts dates in stata to dates and POSIX in R.

#   temp.data.frame <- foreign::read.dta(path,
#                                        convert.dates   = TRUE,
#                                        convert.factors = TRUE)

#   #attr(temp.data.frame, "preview") = obj$preview

#   return(temp.data.frame)
# }

# # #' @rdname .iNZread
# # #' @param sheet the number of the sheet which has to be
# # #' read from the excel workbook.
# # #' @param col.names A logical scalar. Tells if the file
# # #' contains column names in the first row or not.
# # #' @export
# .iNZread.excel <- function(path,
#                            col.types,
#                            sheet     = 1,
#                            col.names = TRUE,
#                            ...) {

#   temp.data.frame <- readxl::read_excel(path,
#                                         sheet     = sheet,
#                                         col_names = col.names,
#                                         col_types = col.types)

#   #attr(temp.data.frame, "preview") = obj$preview

#   return(temp.data.frame)
# }
