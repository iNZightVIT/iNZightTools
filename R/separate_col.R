#' Separate columns
#'
#' @param .data dataset
#' @param col column to be separated
#' @param left name for the separated left column
#' @param right name for the separated right column
#' @param sep separator used to separate columns
#' @param check method of separating
#'
#' @return separated dataset
#' @export
#' @author Yiwen He
separate <- function(.data, col, left, right, sep, check) {
    mc <- match.call()
    dataname <- mc$.data

    punctlist <- c("\\", "[", "(", "{", "?", "^", "$", ")", "]", "}", ".")
    if (sep %in% punctlist) {
        sep <- paste0("\\", sep)
    }

    if (check == "Column") {
        exp <- ~.DATA %>%
            tidyr::separate(
                col = col_name,
                into = into_cols,
                sep = separator,
                extra = "merge"
            )
    } else if (check == "Row") {
        exp <- ~.DATA %>%
            tidyr::separate_rows(col = col_name, sep = separator)
    }

    exp <- replaceVars(exp,
        .DATA = dataname
    )

    interpolate(exp,
        col_name = col,
        into_cols = c(left, right),
        separator = sep
    )
}
