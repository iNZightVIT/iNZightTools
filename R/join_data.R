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

  if (((origin_join_col == "") & (import_join_col !="")) | ((origin_join_col != "") & (import_join_col ==""))) {
    stop('Need same number of cols')
  }
  
  col_names = ""
  if ( (origin_join_col != "") & (import_join_col != "")) {
    for (i in 1:length(origin_join_col)) {
      col_names = paste0(col_names, "'", origin_join_col[i], "'='", import_join_col[i], "',")
    }
    col_names = substr(col_names, 1, nchar(col_names)-1)
  }

  byfml <- ""
  if (col_names != "") {
    byfml <- sprintf(", by = c(%s)", col_names)
  }
  
  suf = paste0(", suffix = c('.", left, "', '.", right, "')")

  exp = ~.DATA %>% .FUN(.IMP.BY.SUFFIX)

  
  exp <- replaceVars(exp, 
                     .DATA = dataname,
                     .IMP = importname,
                     .BY = byfml,
                     .FUN = join_method,
                     .SUFFIX = suf)
  
  interpolate(exp)
}
