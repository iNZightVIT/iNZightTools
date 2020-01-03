#' Reshaping dataset from wide to long or from long to wide
#'
#' @param .data dataset
#' @param col1 column to spread out (for long to wide)
#' @param col2 values to be put in the spread out column (for long to wide)
#' @param cols columns(s) to gather together (for wide to long)
#' @param key name for new column containing old column names (for wide to long)
#' @param value name for new column containing old column values
#'        (for wide to long)
#' @param check check whether to use long to wide or wide to long
#'
#' @return reshaped dataset
#' @author Yiwen He
#' @export
reshape_data <- function(.data, col1, col2, cols, key, value, check) {
    mc <- match.call()
    dataname <- mc$.data

    keynameL <- paste0("key = '", col1, "', ")

    valueL <- paste0("value = '", col2, "'")

    colnames <- ""
    for (i in seq_along(cols)) {
        colnames <- paste0(colnames, "'", cols[i], "'", ", ")
    }

    keynameW <- paste0("key = '", key, "', ")

    valueW <- paste0("value = '", value, "'")

    mutation <- ""
    if (check == "long") {
        exp <- ~.DATA %>%
            tidyr::spread(.KEYL.VALUEL)
    } else if (check == "wide") {
        mutation <- ".VNAME = as.factor(.VNAME)"
        cat_cols <- sapply(cols, function(c) is_cat(.data[[c]]))
        if (any(cat_cols)) {
            mutation <- paste0(mutation, ", .RNAME = as.factor(.RNAME)")
        }
        exp <- ~.DATA %>%
            tidyr::gather(.COL.KEYW.VALUEW) %>%
            dplyr::mutate(.MUTATION)
    }

    exp <- replaceVars(exp,
        .DATA = dataname,
        .KEYL = keynameL,
        .VALUEL = valueL,
        .COL = colnames,
        .KEYW = keynameW,
        .VALUEW = valueW,
        .MUTATION = mutation,
        .VNAME = key,
        .RNAME = value
    )

    interpolate(exp)
}
