unite <- function(.data, name, col, sep) {
  
  mc <- match.call()
  dataname <- mc$.data
  
  colnames <- ", c("
  for (i in 1:length(col)) {
    colnames = paste0(colnames, "'", col[i], "'", ", ")
  }
  colnames = paste0(substr(colnames, 1, nchar(colnames)-2), ")") 
  
  sep <- paste0(", sep = '", sep, "'")
  
  remove <- ", remove = FALSE"
  
  exp = ~.DATA %>%
    tidyr::unite(.NAME.COL.SEP.REMOVE)
  
  exp <- replaceVars(exp,
                     .DATA = dataname,
                     .NAME = name,
                     .COL = colnames,
                     .SEP = sep,
                     .REMOVE = remove)
  
  interpolate(exp)
}