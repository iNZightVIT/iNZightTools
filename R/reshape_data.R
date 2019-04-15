#' Reshaping dataset from wide to long or from long to wide
#'
#' @param .data dataset
#' @param col1 column to spread out (for long to wide)
#' @param col2 values to be put in the spread out column (for long to wide)
#' @param cols columns(s) to gather together (for wide to long)
#' @param key name for new column containing old column names (for wide to long)
#' @param value name for new column containing old column values (for wide to long)
#' @param check check whether to use long to wide or wide to long
#'
#' @return reshaped dataset
#' @author Yiwen He
#' @export
#'
#' @examples
reshape_data <- function(.data, col1, col2, cols, key, value, check) {
  
  mc <- match.call()
  dataname <- mc$.data
  
  keynameL <- paste0("key = '", col1, "', ")
  
  valueL <- paste0("value = '", col2, "'")
  
  colnames <- ""
  for (i in 1:length(cols)) {
    colnames = paste0(colnames, "'", cols[i], "'", ", ")
  }
  
  keynameW <- paste0("key = '", key, "', ")
  
  valueW <- paste0("value = '", value, "'")
  
  
  if (check == "long") {
    exp = ~.DATA %>%
      tidyr::spread(.KEYL.VALUEL)
  } else if (check == "wide") {
    exp = ~.DATA %>%
      tidyr::gather(.COL.KEYW.VALUEW)
  }
  
  exp <- replaceVars(exp,
                     .DATA = dataname,
                     .KEYL = keynameL,
                     .VALUEL = valueL,                     
                     .COL = colnames,
                     .KEYW = keynameW,
                     .VALUEW = valueW
                     )
  
  interpolate(exp)
}

