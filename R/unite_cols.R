#' Unite columns in a dataset
#'
#' @param .data dataset
#' @param name name for the new united column
#' @param col a vector of column names
#' @param sep separator used in between the united columns
#'
#' @return united dataset
#' @export
#' @author Yiwen He
unite <- function(.data, name, col, sep) {
    mc <- match.call()
    dataname <- mc$.data

    colnames <- ", c("
    for (i in seq_along(col)) {
        colnames <- paste0(colnames, "'", col[i], "'", ", ")
    }

    colnames <- paste0(substr(colnames, 1, nchar(colnames) - 2), ")")

    sep <- paste0(", sep = '", sep, "'")

    remove <- ", remove = FALSE"

    fmla <- "tidyr::unite(.NAME.COL.SEP.REMOVE)"
    if (is_survey(.data)) {
        exp <- ~.DATA %>%
            {
                d <- (.)
                d$variables <- d$variables %>% .FMLA
                d
            }
    } else {
        exp <- ~.DATA %>% .FMLA
    }

    exp <- replaceVars(exp,
        .FMLA = fmla,
        .DATA = dataname,
        .NAME = name,
        .COL = colnames,
        .SEP = sep,
        .REMOVE = remove
    )

    interpolate(exp)
}
