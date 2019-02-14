appendrows <- function(.data, imported_data) {
  
  mc <- match.call()
  dataname <- mc$.data
  importname <- mc$imported_data

  exp = ~.DATA %>% bind_rows(.IMP)
  
  
  exp <- replaceVars(exp, 
                     .DATA = dataname,
                     .IMP = importname)
  
  interpolate(exp)
}
