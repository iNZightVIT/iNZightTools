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

    fmla <- ".COLTRANStidyr::unite(.NAME, .COL, sep = .SEP, remove = FALSE)"
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

    f_cols <- col
    .fdata <- if (is_survey(.data)) .data$variables else .data
    f_cols <- f_cols[sapply(.fdata[f_cols], function(x) is.factor(x) && any(is.na(x)))]
    f_trans <- ""
    if (length(f_cols)) {
        f_trans <- paste(
            f_cols, " = forcats::fct_explicit_na(", f_cols, ", 'NA')",
            collapse = ", "
        )
        f_trans <- sprintf("dplyr::mutate(%s) %s ", f_trans, "%>%")
    }

    exp <- replaceVars(exp,
        .FMLA = fmla,
        .COLTRANS = f_trans,
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
