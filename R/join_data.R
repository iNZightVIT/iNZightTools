joindata <- function(.data, imported_data, origin_join_col, import_join_col, join_method) {
  mc <- match.call()
  dataname <- mc$.data
  
  exp = switch(join_method, "Inner Join" = "inner_join",
               "Left Join" = "left_join",
               "Full Join" = "full_join",
               "Semi Join" = "semi_join",
               "Anti Join" = "anti_join")
  
  
  
  
  formulae = list(~.DATA)
  
  for (i in 1:length(vars)){
    formula <- ~tibble::add_column(.NAME = factor(.DATA$.VARNAME), .after = ".VARNAME")
    formula <- replaceVars(formula, .VARNAME = vars[i], .NAME = names[i])
    formulae[[i+1]] <- formula
  }
  
  exp <- pasteFormulae(formulae)
  exp <- replaceVars(exp, .DATA = dataname)
  
  interpolate(exp)