#' Select variables from a dataset
#'
#' @param .data the dataset
#' @param keep vector of variable names to keep
#' @return a data frame
#' @author Tom Elliott
#' @export
#' @examples
#' selectVars(iris, c("Sepal.Length", "Species", "Sepal.Width")
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
