## Run the script and use the function readMetadata. Takes the argument path. 
## Returns a list. 
## Author - Akshay Gupta

pattern.vector <- c("[#@]", "factor|numeric|date")
variable.specs <- list()

#' Extracts the levels and labels to be used when converting a column to factor.
#'
#' \code{convertToLevelsAndLabels} Returns a list with levels vector and labels vector.
#'
#' @param obj 
#' @return A list.
convertToLevelsAndLabels <- function(obj){
  
  labels.vector <- c()
  levels.vector <- c()
  
  for (i in 1:length(obj)){
    
    value.vector <- solveForValues(obj[i])

    if(length(value.vector) != 1){
      
      value.labels <- rep(value.vector[1], length(value.vector) - 1)
      labels.vector <- c(labels.vector, value.labels)
      levels.vector <- c(levels.vector, value.vector[-1])
    }
    else{
      
      labels.vector <- c(labels.vector, value.vector[1])
    }
  }
  
  values.list <-  list(levels.vector = levels.vector, labels.vector = labels.vector)
  return(values.list)
  
}

#' Extracts the levels and labels to be used when converting a column to factor.
#'
#' \code{readMetadata} Returns a vector with label and levels. 
#'
#' @param obj A string.
#' @return A character vector.
solveForValues <- function(obj){
  
  UseMethod("solveForValues")
}

#' @rdname solveForValues
solveForValues.default <- function(obj){
  
  check <- grepl("(([a-z]|[A-Z]|[1-9])*)=(([a-z]|[A-Z]|[1-9])*)", obj)
  #print(check)
  
  if(check){
    
    class(obj) <- "with.levels"
  }
  else{
    
    class(obj) <- "without.levels"
  }
  
  solveForValues(obj)
}

#' @rdname solveForValues
solveForValues.with.levels <- function(obj){
  
  #formatted.string <- gsub('(\\(([a-z]|[A-Z]|[1-9])*),(([a-z]|[A-Z]|[1-9])*\\))', '\\1?\\3', string )
  values.vector <- unlist(strsplit(obj, split = "="))
  values.levels <- values.vector[2]
  
  check <- grepl("(\\(([a-z]|[A-Z]|[1-9])*)?(([a-z]|[A-Z]|[1-9])*\\))", values.levels)
  
  if(check){
    
    formatted.values.levels <- substr(values.levels, 2, nchar(values.levels) - 1)
    formatted.values.levels.vector <- unlist(strsplit(formatted.values.levels, split = "\\?"))
    values.vector <- c(values.vector[1], formatted.values.levels.vector)
  }
  
  return(values.vector)
}

#' @rdname solveForValues
solveForValues.without.levels <- function(obj){
  
  return(obj)
}

#' Remove multiple spaces and replace with a single space.
#'
#' \code{remDoubleSpaces} Returns a formatted string with 
#' multiple spacces substituted with a single space.
#'
#' @param string A string. A line in the metadata.
#' @return A string.
remDoubleSpaces <- function(string){
  
  formatted.string <- gsub('\\s\\s+', ' ', string)
  return(formatted.string)
}

#' Remove spaces around operators like '=','+','-', and back-slash.
#'
#' \code{remSpacesAroundOperators} Returns a formatted string with 
#' spaces around operators listed removed.
#'
#' @param string A string. A line in the metadata.
#' @return A string.
remSpacesAroundOperators <- function(string){
  
  formatted.string <- gsub('\\s*([,\\[\\=\\+\\-\\\\])\\s*','\\1', string, perl =TRUE)
  return(formatted.string)
}

#' Removes unneeded spaces like double spaces or spaces around operators.
#'
#' \code{removeUnneededSpaces} Returns a formatted string with 
#' unneeded spaces removed.
#'
#' @param string A string. A line in the metadata.
#' @return A string.
removeUnneededSpaces <- function(string){
  
  formatted.string <- remDoubleSpaces(string)
  formatted.string = remSpacesAroundOperators(formatted.string)
  return(formatted.string)
}

#' Replaces comma inside factor's values with '?'. 
#'
#' \code{replaceComma} Returns a string with '?' instead of ',' inside 
#' brackets for factor values.
#'
#' @param string A string. A line in the metadata.
#' @return A string.
replaceComma <- function(string){
  
  formatted.string <- gsub('(\\(([a-z]|[A-Z]|[1-9])*),(([a-z]|[A-Z]|[1-9])*\\))', '\\1?\\3', string )
  return(formatted.string)
}

#' Converts the string into a list of words. 
#'
#' \code{returnWords} Returns a list of words present in the line.
#'  
#' @param string A string. A line in the metadata.
#' @return A list.
returnWords <- function(string){
  
  #Formats the query and returns words.   
  formatted.string <- removeUnneededSpaces(string)
  line <- strsplit(formatted.string, split = " ")
  unlisted.line <- unlist(line)
  words <- strsplit(unlisted.line, split= " ") 
  
  return(words)
  
}

#' Solves the first word in the metadata line.
#'
#' \code{solveSecondWord} Returns variable name if correct metadata is present. 
#' Returns FALSE otherwise. 
#'
#' @param first.word A string. First word/expression/phrase in the metadata line.
#' @return A string or a logical vector of length 1.
solveFirstWord <- function(first.word){
  
  check <- grepl(pattern.vector[1], first.word)
  
  if(check == TRUE)
  {
    #Get name of the variable.
    variable <- substr(first.word, 3, nchar(first.word))

    return(variable)
  }
  else{
    
    return(FALSE)
  }
}

#' Solves the second word in the metadata line which can be of the 
#' form numeric,character, factor["x=xyz"]. It adds the details of the metadata in a list.
#'
#' \code{solveSecondWord} Returns a list updated with the datatype of the column 
#' and its values as an attribute if available.
#'
#' @param second.word A string. Second word/expression/phrase in the metadata line.
#' @return A list.
solveSecondWord <- function(second.word, variable.specs){

  check.line <- regexec(pattern.vector[2], second.word)
  datatype <- substr(second.word, check.line[[1]], 
                     attr(check.line[[1]], "match.length") + check.line[[1]] - 1)
  class(second.word) <- paste("iNZmeta", datatype, sep = '.')

  UseMethod("solveSecondWord", second.word)
}

#' @rdname solveSecondWord
solveSecondWord.default <- function(second.word, variable.specs){
  
  #Do something here. (query not supported)
  print("ENTERED DEFAULT")
}

#' @rdname solveSecondWord
solveSecondWord.iNZmeta.factor <- function(second.word, variable.specs){
  
  attr(variable.specs[[length(variable.specs)]], "datatype") <- "factor"
  
  #Add values to the attribute of variable.
  values.line <- substr(second.word, 8, nchar(second.word) - 1)

  formatted.values.line <- replaceComma(values.line)
  
  values.list <- strsplit(formatted.values.line, split = ",")
  attr(variable.specs[[length(variable.specs)]], "values") <- values.list

  return(variable.specs)
}

#' @rdname solveSecondWord
solveSecondWord.iNZmeta.numeric <- function(second.word, variable.specs){

  attr(variable.specs[[length(variable.specs)]], "datatype") <- substr(second.word, 1, 7)

  return(variable.specs)
}

#' @rdname solveSecondWord
solveSecondWord.iNZmeta.date <- function(second.word, variable.specs){
  
  attr(variable.specs[[length(variable.specs)]], "datatype") <- substr(second.word, 1, 4)
  
  return(variable.specs)
}

#' Reads a file and checks if metadata is available.
#'
#' \code{isMetadataAvailable} Returns TRUE if metadata is available.
#'
#' @param path A string. Specifies the location of the file to be read.
#' @return A logical vector of length 1.
isMetadataAvailable <- function(path){
  
  con <- file(description = path, open="r")
  tmp <- readLines(con, n=1)
  words <- returnWords(tmp)
  
  variable.name <- solveFirstWord(words[[1]])
  
  close(con)
  
  if(is.logical(variable.name)){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
  
}

#' Checks if all the column names specified in the metadata 
#' are present in the header line or not.
#'
#' \code{checkForColumnNames} Returns TRUE if all the columns specified 
#' in the metadata are present in the first line which is not part of 
#' the metadata.
#'
#' @param metadata.list Metadata passed as a list.
#' @param first.line A string. First line which is not part of the metadata.
#' @return A logical vector of length 1. 
checkForColumnNames <- function(metadata.list, first.line){
  
  first.line.list <- strsplit(first.line, split = ",")
  
  columns.exist <- lapply(metadata.list, function(x){
    
    if(x[[1]] %in% first.line.list[[1]]){
      TRUE
    }
    else{
      FALSE
    }
  })
  
  return(all(columns.exist))  
}

#' Reads a file and returns the first line which isn't a part of the metadata.
#'
#' \code{findColumnNames} Returns the first line which isn't a part of metadata.
#'
#' @param path A string. Specifies the location of the file to be read.
#' @return A string.
findColumnNames <- function(path){
  
  con <- file(description = path, open="r")
  
  while(con){
    
    tmp <- readLines(con, n = 1)
    
    check <- grepl("[#]", tmp)
    
    if(!check){
      #print(tmp)
      return(tmp)
    }
    
  }
}

#' Reads a file and checks for metadata if available. 
#'
#' \code{readMetadata} Returns the metadata if available in the file as a list.
#'
#' @title iNZight Metadata Reader
#' @param path A string. Specifies the location of the file to be read.
#' @return A list.
#'
#' @author Akshay Gupta
readMetadata <- function(path){
  
  variable.specs <- list()
  con <- file(description = path, open="r")
  
  while(con){
    
    tmp <- readLines(con, n = 1)
    words <- returnWords(tmp)
    
    variable.name <- solveFirstWord(words[[1]])

    if(is.logical(variable.name)){
      break
    }
    else{
      variable.specs[length(variable.specs) + 1] <- variable.name
    }
    
    new.variable.specs <- solveSecondWord(words[[2]], variable.specs)
    variable.specs <- new.variable.specs
  }
  
  close(con)
  return(variable.specs)
}