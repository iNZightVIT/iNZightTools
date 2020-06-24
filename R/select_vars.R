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

    exp <- ~.DATA %>% dplyr::select(.KEEP)
    exp <- replaceVars(exp,
        .DATA = dataname,
        .KEEP = keep
    )

    interpolate(exp)
}
