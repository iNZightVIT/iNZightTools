#' Delete variables from a dataset
#'
#' @title Delete variables
#' @param .data dataset
#' @param vars variables to delete
#' @return dataset without chosen variables
#' @author Tom Elliott
#' @export
deleteVars <- function(.data, vars) {
    mc <- match.call()
    dataname <- mc$.data

    if (is_survey(.data)) {
        todelete <- paste(vars, " = NULL", collapse = ", ")
        exp <- ~.DATA %>% update(.VARS)
    } else {
        todelete <- paste("-", vars, collapse = ", ")
        exp <- ~.DATA %>% dplyr::select(.VARS)
    }
    exp <- replaceVars(exp,
        .VARS = todelete,
        .DATA = dataname
    )
    interpolate(exp)
}
