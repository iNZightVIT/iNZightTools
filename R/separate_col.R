#' Separate columns
#'
#' @param .data dataset
#' @param col column to be separated
#' @param left name for the separated left column
#' @param right name for the separated right column
#' @param sep separator used to separate columns
#' @param check method of separating
#'
#' @return separated dataset
#' @export
#' @author Yiwen He
separate <- function(.data, col, left, right, sep, check) {

  mc <- match.call()
  dataname <- mc$.data

  if (sep != "") {
    sep_string <- paste0(", sep = '", sep, "'")
  }

  name <- paste0(", into = c('", left, "', '", right, "')")

  if (check == "Row") {
    name = ""
  }

  exp <- ~.DATA %>%
    .FUN(.COL.NAME.SEP.MERGE)

  if (check == "Column") {
    exp <- ~.DATA %>%
      tidyr::separate(.COL.NAME.SEP.MERGE)
  } else if (check == "Row") {
    exp <- ~.DATA %>%
      tidyr::separate_rows(.COL.SEP)
  }


  exp <- replaceVars(exp,
                     .DATA = dataname,
                     .COL = col,
                     .NAME = name,
                     .SEP = sep_string,
                     .MERGE = ", extra = 'merge'",
                     .FILL = ", fill = 'right'")

  interpolate(exp)
}
