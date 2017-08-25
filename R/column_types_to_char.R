#' Converts the column datatypes to a single word or string which can be
#' passed to readr::read_csv as an argument.
#'
#' \code{makeDatatypeChar} returns a word (string) specifiying the datatype
#' of each column with a single character.
#'
#' @param obj A character vector or a list.
#' @param col.names names of columns, optional
#' @return A character vector of length 1.
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
                                    "categorical" = "c",
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
                                      "categorical" = "c",
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
