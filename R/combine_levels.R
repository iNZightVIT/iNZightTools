#' Combine the levels of factor variables.
#'
#' @param dafr The data.frame containing factor columns
#'        specified in columns.
#' @param columns The column names of the columns in dafr
#'        to combine.
#' @return A data.frame with one additional column as dafr,
#' which contains the combined levels.
#'
#' @author Christoph Knapp
combine.levels <- function(dafr, columns) {
    new.column <- do.call(paste,
        lapply(columns,
            function(name, d) {
              d[, name]
            },
            dafr
        )
    )
    dafr <- cbind(dafr, gsub(" ", ".", new.column))
    colnames(dafr)[ncol(dafr)] <- paste(columns, collapse = ".")
    dafr
}
