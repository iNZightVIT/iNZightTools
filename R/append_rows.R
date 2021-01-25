#' Append row to the dataset
#'
#' @param .data original dataset
#' @param imported_data imported dataset
#' @param date whether a "When_Added“ column is required (deafult `FALSE`)
#'
#' @return dataset with new rows appended
#' @md
#' @export
#' @author Yiwen He
appendrows <- function(.data, imported_data, date = FALSE) {

    mc <- match.call()
    dataname <- mc$.data
    importname <- mc$imported_data

    if (is_survey(.data))
        stop("Cannot append rows to surveys")

    if (date) {
        exp <- ~.DATA %>%
            dplyr::bind_rows(
                .IMP %>% tibble::add_column("When_Added" = Sys.time())
            )
    } else {
        exp <- ~.DATA %>% dplyr::bind_rows(.IMP)
    }

    exp <- replaceVars(exp,
        .DATA = dataname,
        .IMP = importname
    )

    interpolate(exp)
}
