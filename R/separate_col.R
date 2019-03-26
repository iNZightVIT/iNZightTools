separate <- function(.data, col, left, right, sep, check) {
  
  mc <- match.call()
  dataname <- mc$.data
  
  method <- switch(check, "Column" = "separate",
                          "Row" = "separate_rows")
  
  name <- paste0(", into = c('", left, "', '", right, "')")
  
  if (sep != "") {
    sep <- paste0(", sep = '", sep, "'")
  }
  
  if (check == "Column") {
    exp = ~.DATA %>% 
      .FUN(.COL.NAME.SEP)
  } else {
    exp = ~.DATA %>%
      .FUN(.COL.SEP)
  }
  
  exp <- replaceVars(exp,
                     .DATA = dataname,
                     .FUN = paste("tidyr", method, sep = "::"),
                     .COL = col,
                     .NAME = name,
                     .SEP = sep)
  
  interpolate(exp)
}