reshape_data_wide_to_long <- function(.data, cols, colname, value) {
  mc <- match.call()
  dataname <- mc$.data

  colnames <- ""
  for (i in 1:length(cols)) {
    colnames = paste0(colnames, "'", cols[i], "'", ", ")
  }
  
  keyname <- paste0("key = '", colname, "', ")
  
  value <- paste0("value = '", value, "'")
  
  exp = ~.DATA %>%
    tidyr::gather(.COL.KEY.VALUE)

  exp <- replaceVars(exp,
                     .DATA = dataname,
                     .COL = colnames,
                     .KEY = keyname,
                     .VALUE = value)
  
  interpolate(exp)
}
