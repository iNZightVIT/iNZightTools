#' Append row to the dataset
#'
#' @param .data original dataset
#' @param imported_data imported dataset
#' @param colnames column names of the original dataset
#' @param date whether a "When_Added“ column is required
#'
#' @return dataset with new rows appended
#' @export
#' @author Yiwen He
appendrows <- function(.data, imported_data, colnames, date) {

  mc <- match.call()
  dataname <- mc$.data
  importname <- mc$imported_data

  if (date) {
    exp = ~.DATA %>%
      dplyr::bind_rows(.IMP %>% tibble::add_column("When_Added" = Sys.time()))
  } else {
    exp = ~.DATA %>% dplyr::bind_rows(.IMP)
  }

  exp <- replaceVars(exp,
                     .DATA = dataname,
                     .IMP = importname)

  interpolate(exp)
}


?tibble::add_column
