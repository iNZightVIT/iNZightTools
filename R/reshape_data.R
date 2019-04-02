#' Reshape long to wide
#'
#' Reshape a data set from long form (observations in rows),
#' to wide form (observations in columns).
#'
#' @param .data a dataframe or tibble to be reshaped
#' @param col1 the column containing the 'key'
#' @param col2 the column containing the values
#' @return a dataframe or tibble
#' @author Yiwen He
#' @export
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

#' Reshape wide to long
#'
#' Reshape a data set from wide form (observations in columns)
#' to long form (observations in rows)
#'
#' @param .data a dataframe or tibble to be reshaped
#' @param cols the columns containing values
#' @param colname the name of the 'key' column in the new data set
#' @param value the name of the 'value' column in the new data set
#' @return a dataframe or tibble
#' @author Yiwen He
#' @export
reshape_data_wide_to_long <- function(.data, cols, colname, value) {
  mc <- match.call()
  dataname <- mc$.data

  colnames <- ""
  for (i in 1:length(cols)) {
    colnames = paste0(colnames, "'", cols[i], "'", ", ")
  }

  keyname <- paste0("key = '", colname, "', ")

  value <- paste0("value = '", value, "'")

  exp = ~.DATA %>%
    tidyr::gather(.COL.KEY.VALUE)

  exp <- replaceVars(exp,
                     .DATA = dataname,
                     .COL = colnames,
                     .KEY = keyname,
                     .VALUE = value)

  interpolate(exp)
}
