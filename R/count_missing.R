#' Count missing values
#'
#' @param var the vector to sum up the number of missing values
#' @param na.rm ignore this
#' @return the number of missing values for that vector
#' @seealso \code{\link{aggregateData}}
#' @author Owen Jin
#' @export
countMissing <- function(var, na.rm = FALSE) {
    sum(is.na(var))
}
