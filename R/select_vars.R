#' Select variables from a dataset
#'
#' Select a (reordered) subset of variables from a subset.`
#'
#' @param .data the dataset
#' @param keep vector of variable names to keep
#' @return a data frame with tidyverse code attribute
#' @author Tom Elliott
#' @examples
#' selectVars(iris, c("Sepal.Length", "Species", "Sepal.Width"))
#' @export
selectVars <- function(.data, keep) {
    mc <- match.call()
    dataname <- mc$.data

    keep <- paste(keep, collapse = ", ")

    if (is_survey(.data) && !inherits(.data, "tbl_svy")) {
        .data <- srvyr::as_survey(.data)
        dataname <- glue::glue("{dataname} %>% srvyr::as_survey()")
    }

    exp <- ~.DATA %>% dplyr::select(.KEEP)
    exp <- replaceVars(exp,
        .DATA = dataname,
        .KEEP = keep
    )

    interpolate(exp)
}
