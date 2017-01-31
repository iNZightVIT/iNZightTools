
#' Reads the file and returns a dataframe according to the arguments
#' passed.
#'
#' \code{.makeDatatypeChar} returns a dataframe by converting the data in the
#' file passed based on the arguments included while passing the file.
#'
#' @title iNZight Import Data
#' @param path A string. Specifies the location of the file to be read.
#' @param ... additional arguments
#' @return A dataframe.
#'
#' @author Akshay Gupta
makeDatatypeChar <- function(obj, col.names = NULL){
  
  UseMethod("makeDatatypeChar")
}

#' @rdname makeDatatypeChar
makeDatatypeChar.character <- function(obj){
  
  types.list <- lapply(obj, function(x){
    column.type.character <- switch(x,
                                    "numeric"   = "n",
                                    "character" = "c",
                                    "factor"    = "c",
                                    "date"      = "D",
                                    "date-time" = "T",
                                                  { "?" })
  })
  
  types.vector <- sapply(types.list, paste, collapse = ",")
  col.types <- paste(types.vector, collapse = "")
  
  return(col.types)
}

#' @rdname makeDatatypeChar
makeDatatypeChar.list <- function(obj, col.names){
  
  types.list <- lapply(col.names, function(x){
    if ( x %in% obj){
      
      column.type.character <- switch(attr(obj[obj == x][[1]], "datatype"),
                                      "numeric"   = "n",
                                      "character" = "c",
                                      "factor"    = "c",
                                      "date"      = "D",
                                      "date-time" = "T",
                                                    { "?" })
    }
    else{
      
      column.type.character <- "?"
    }
  })
  
  types.vector <- sapply(types.list, paste, collapse = ",")
  col.types <- paste(types.vector, collapse = "")
  
  return(col.types)
}
