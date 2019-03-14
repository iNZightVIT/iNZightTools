reshape_data <- function(.data, cols, colname, value) {
  mc <- match.call()
  dataname <- mc$.data
  
  print(cols)
  print(colname)
  
  colnames <- ""
  for (i in 1:length(cols)) {
    colnames = paste0(colnames, "'", cols[i], "'", ", ")
  }
  
  keyname <- paste0("key = '", colname, "', ")
  
  value <- paste0("value = '", value, "'")
  
  print(keyname)
  print(value)
  
  exp = ~.DATA %>%
    gather(.COL.KEY.VALUE)
  
  print(exp)
  
  exp <- replaceVars(exp,
                     .DATA = dataname,
                     .COL = colnames,
                     .KEY = keyname,
                     .VALUE = value)
  
  interpolate(exp)
}
