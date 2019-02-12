joindata <- function(.data, imported_data, origin_join_col, import_join_col, join_method = "inner", left = ".x", right = ".y") {
  
  mc <- match.call()
  dataname <- mc$.data
  importname <- mc$imported_data
  
  # join cols need to have the same length (if not, stop('NEed same number of cols'))
  
  col_names = ""
  if (!missing(origin_join_col) && !missing(import_join_col)) {
    for (i in 1:length(origin_join_col)) {
      col_names = paste0(col_names, "'", origin_join_col[i], "'='", import_join_col[i], "',")
    }
    col_names = substr(col_names, 1, nchar(col_names)-1)
  }
  
  fun <- sprintf("%s_join", join_method)
  # expext = switch(join_method, "Inner Join" = "inner_join",
  #                 "Left Join" = "left_join",
  #                 "Full Join" = "full_join",
  #                 "Semi Join" = "semi_join",
  #                 "Anti Join" = "anti_join")
  # expext = paste0(expext, '(', '.IMP', ", by = c(", col_names, "), suffix = c('.", '.LEFT', "', '.", '.RIGHT', "'))")
  
  # print(paste0(.EXP, '(', .DATA, ', ', .DATA2, ", by = c(", col_names, "))"))
  
  ## Fname = paste0(.EXP, '(', .DATA, ', ', .DATA2, ", by = c(", .COL, "))")
  
  print(expext)
  print(dataname)
  print(importname)
  
  byfml <- ""
  if (length(col_names)) {
    byfml <- sprintf(", by = c(%s)", col_names)
  }
  
  
  exp = ~.DATA %>% .FUN(.IMP.BY.METHOD.SUFFIX)
  
  exp <- replaceVars(exp, 
                     .EXP = expext,
                     .DATA = dataname,
                     .IMP = importname,
                     .LEFT = left,
                     .RIGHT = right)
  
  interpolate(exp)
}

# joindata <- function(.data, imported_data, origin_join_col, import_join_col, join_method, left, right) {
#   
#   mc <- match.call()
#   dataname <- mc$.data
#   
#   col_names = ""
#   for (i in 1:length(origin_join_col)) {
#     col_names = paste0(col_names, "'", origin_join_col[i], "'='", import_join_col[i], "',")
#   }
#   col_names = substr(col_names, 1, nchar(col_names)-1)
#   
#   expext = switch(join_method, "Inner Join" = "inner_join",
#                   "Left Join" = "left_join",
#                   "Full Join" = "full_join",
#                   "Semi Join" = "semi_join",
#                   "Anti Join" = "anti_join")
#   expext = paste0(expext, '(', '.DATA', ', ', '.IMP', ", by = c(", col_names, "), suffix = c('.", '.LEFT', "', '.", '.RIGHT', "'))")
#   
#   print(expext)
#   print(.data)
#   print(imported_data)
#   
#   exp = ~.DATA %>% .EXP
#   
#   exp <- replaceVars(exp, 
#                      .EXP = expext,
#                      .DATA = dataname,
#                      .IMP = imported_data,
#                      .LEFT = left,
#                      .RIGHT = right)
#   
#   interpolate(exp)
# }