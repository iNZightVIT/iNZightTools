reshape_data_long_to_wide <- function(.data, col1, col2) {
  
  mc <- match.call()
  dataname <- mc$.data
  
  keyname <- paste0("key = '", col1, "', ")
  
  value <- paste0("value = '", col2, "'")
  
  exp = ~.DATA %>%
    tidyr::spread(.KEY.VALUE)
  
  exp <- replaceVars(exp,
                     .DATA = dataname,
                     .KEY = keyname,
                     .VALUE = value)
  
  interpolate(exp)
}
