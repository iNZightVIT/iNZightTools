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

    fmla <- "tidyr::unite(.NAME, .COL, sep = .SEP, remove = FALSE)"
    if (is_survey(.data)) {
        exp <- ~.DATA %>%
            {
                d <- (.)
                d$variables <- d$variables %>% .FMLA %>% .FACTOR
                d
            }
    } else {
        exp <- ~.DATA %>% .FMLA %>% .FACTOR
    }

    exp <- replaceVars(exp,
        .FMLA = fmla,
        .FACTOR = "dplyr::mutate(.VNAME = as.factor(.VNAME))",
        .DATA = dataname,
        .VNAME = name
    )

    interpolate(exp,
        .NAME = name,
        .COL = col,
        .SEP = sep
    )
}
