appendrows <- function(.data, imported_data, date) {
  
  mc <- match.call()
  dataname <- mc$.data
  importname <- mc$imported_data
  
  if (date) {
    exp = ~.DATA %>% 
      bind_rows(.IMP) %>% 
      tibble::add_column("When_Added" = Sys.time())
  } else {
    exp = ~.DATA %>% bind_rows(.IMP)
  }

  exp <- replaceVars(exp, 
                     .DATA = dataname,
                     .IMP = importname)
  
  interpolate(exp)
}
