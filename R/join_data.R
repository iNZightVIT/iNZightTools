#' Join data with another dataset
#'
#' @param .data Original data
#' @param imported_data Imported dataset
#' @param origin_join_col column selected from the original data
#' @param import_join_col column selected from the imported dataset
#' @param join_method function used to join the two datasets
#' @param left suffix name assigned to the original dataset
#' @param right suffix name assigned to the imported dataset
#'
#' @return joined dataset
#' @export
#'
#' @examples
joindata <- function(.data, imported_data, origin_join_col, import_join_col, join_method, left, right) {
  
  mc <- match.call()
  dataname <- mc$.data
  importname <- mc$imported_data
  
  for (i in 1:length(origin_join_col)) {
    if (((origin_join_col[i] == "") & (import_join_col[i] !="")) | ((origin_join_col[i] != "") & (import_join_col[i] ==""))) {
      stop('Must select at least one column from each dataset to match on')
    }
  }
  
  col_names = ""
  for (i in 1:length(origin_join_col)) {
    if ((origin_join_col[i] != "") & (import_join_col[i] != "")) {
        col_names = paste0(col_names, "'", origin_join_col[i], "'='", import_join_col[i], "',")
    }
  }
  col_names = substr(col_names, 1, nchar(col_names)-1)

  byfml <- ""
  if (col_names != "") {
    byfml <- sprintf(", by = c(%s)", col_names)
  }
  
  suf = paste0(", suffix = c('.", left, "', '.", right, "')")
  
  exp = ~.DATA %>% 
    .FUN(.IMP.BY.SUFFIX)
  
  exp <- replaceVars(exp, 
                     .DATA = dataname,
                     .IMP = importname,
                     .BY = byfml,
                     .FUN = join_method,
                     .SUFFIX = suf)

  if ((origin_join_col == "") && (import_join_col == "")) {
    vars = testthat::capture_message(interpolate(exp))
    vars = stringi::stri_extract_all_regex(vars, '"\\S+"')
    import_col = list()
    for (i in 1:length(vars[[1]])) {
      import_col = append(import_col, noquote(vars[[1]][i]))
    }
    origin_col = import_col
    interpolate(exp)
  } else {
    res <- interpolate(exp)
  }
  # attr(res, "join_cols") <- structure(import_col, .Names = origin_col)
}


  #interpolate(exp)
  
  # if (origin_join_col == "" & import_join_col == "") {
  #   res = interpolate(exp)
  #   vars = Reduce(intersect, list(colnames(.data), colnames(imported_data)))
  #   import_col = vars
  #   origin_col = vars
  # } else {
  #   res = interpolate(exp)
  # }
  # 
  # attr(res, "join_cols") = structure(import_col, .Names = origin_col)
  
  # interpolate(exp)
  
  # if (origin_join_col == "" & import_join_col == "") {
  #   vars = Reduce(intersect, list(colnames(.data), colnames(imported_data)))
  #   import_col = vars
  #   origin_col = vars
  #   attr(exp, "join_cols") = structure(import_col, .Names = origin_col)
  # }
