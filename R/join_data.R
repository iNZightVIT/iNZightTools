joindata <- function(.data, imported_data, origin_join_col, import_join_col, join_method, name) {
  
  mc <- match.call()
  dataname <- mc$.data
  
  col_names = ""
  for (i in 1:length(origin_join_col)) {
    col_names = paste0(col_names, "'", origin_join_col[i], "'='", import_join_col[i], "',")
  }
  col_names = substr(col_names, 1, nchar(col_names)-1)
  
  expext = switch(join_method, "Inner Join" = "inner_join",
                  "Left Join" = "left_join",
                  "Full Join" = "full_join",
                  "Semi Join" = "semi_join",
                  "Anti Join" = "anti_join")
  expext = paste0(expext, '(', .DATA, ', ', .DATA2, ", by = c(", col_names, "))")
  
  ## print(paste0(.EXP, '(', .DATA, ', ', .DATA2, ", by = c(", col_names, "))"))
  
  ## Fname = paste0(.EXP, '(', .DATA, ', ', .DATA2, ", by = c(", .COL, "))")
  
  print(expext)
  
  exp = ~.DATA %>%
    tibble::add_column(.NAME = .EXP)
  
  exp <- replaceVars(exp, .DATA = dataname,
                     .DATA2 = imported_data,
                     .EXP = expext,
                     .NAME = name)
  
  interpolate(exp)
}