#' Takes an object (list or a dataframe) and converts it to metadata.
#' Stores it in a text file "output.txt".
#'
#' \code{iNZexport} Creates a new file "output.txt" with metdata for the object.
#'
#' @title iNZight Metadata Writer
#' @param obj The object whose metadata has to be written.
#'
#' @author Akshay Gupta
#' @export
iNZexport <-  function(obj){

  metadata <- convertToMetadata(obj)
  file.create("output.txt")
  con <- file("output.txt")
  writeLines(metadata, con)
  close(con)
}

#' Takes an object (list or a dataframe) and returns its metadata.
#'
#' \code{convertToMetadata} Returns the metadata for the object passed.
#'
#' @param obj The object whose metadata has to be returned.
#' @return A string - metadata.
convertToMetadata <- function(obj){

  UseMethod("convertToMetadata")
}

#' @rdname convertToMetadata
convertToMetadata.data.frame <- function(user.data.frame){

  metadata.list <- list()

  for (i in 1:length(colnames(user.data.frame))){

    variable.name <- colnames(user.data.frame)[i]
    class.column <- class(user.data.frame[[i]])
    metadata.list[length(metadata.list) + 1] <- variable.name
    attr(metadata.list[[length(metadata.list)]], "datatype") <- class.column
    class(user.data.frame[[i]]) <- paste("iNZclass", class.column, sep = '.')
    new.metadata.list <- extractFromFrame(user.data.frame[[i]], metadata.list)
    metadata.list <- new.metadata.list
  }

  metadata.text <- convertToMetadata(metadata.list)
  return(metadata.text)
}

#' Extracts more information from the dataframe (obj).
#'
#' \code{extractFromFrame} Returns a list.
#'
#' @param obj A vector.
#' @param metadata.list A list with information about the metadata of the object.
#' @return A list.
extractFromFrame <- function(obj, metadata.list){

  UseMethod("extractFromFrame", obj)
}

#' @rdname extractFromFrame
extractFromFrame.iNZclass.factor <- function(obj, metadata.list){

  #print(levels(obj))
  attr(metadata.list[[length(metadata.list)]], "values") <- list(levels(obj))
  #print(metadata.list)
  return(metadata.list)
}

#' @rdname extractFromFrame
extractFromFrame.iNZclass.numeric <- function(obj, metadata.list){

  return(metadata.list)
}

#' @rdname extractFromFrame
extractFromFrame.iNZclass.character <- function(obj, metadata.list){

  return(metadata.list)
}

#' @rdname convertToMetadata
convertToMetadata.list <- function(metadata.list){

  metadata.text <- ""

  for (i in 1:length(metadata.list)){

    variable.name <- metadata.list[[i]][1]
    metadata.variable.list <- metadata.list[[i]]
    datatype <- attr(metadata.variable.list, "datatype")
    #print(datatype)
    class(metadata.variable.list) <- paste("iNZclass", datatype, sep = '.')

    query <- convertToLine(metadata.variable.list)
    metadata.line <- paste("#@", variable.name, " ", query, "\n", sep = "")

    metadata.text <- paste(metadata.text, metadata.line, sep = "")

  }
  return(metadata.text)
}


convertToLine <- function(obj){

  UseMethod("convertToLine")
}

convertToLine.iNZclass.numeric <- function(obj){

  return("numeric")
}

convertToLine.iNZclass.character <- function(obj){

  return("character")
}

convertToLine.iNZclass.factor <- function(obj){

  if("values" %in% names(attributes(obj))){

    for (i in 1:length(attr(obj, "values")[[1]])){

      attr(obj, "values")[[1]][i] <- gsub('(\\(([a-z]|[A-Z]|[1-9])*)\\?(([a-z]|[A-Z]|[1-9])*\\))', '\\1,\\3', attr(obj, "values")[[1]][i])
    }

    query.values <- paste(attr(obj, "values")[[1]], collapse = ",")
    query <- paste("factor", "[", query.values, "]", sep = "")
  }
  else{

    query <- "factor"
  }
  return(query)
}
