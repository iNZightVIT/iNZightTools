#' Rename column names
#'
#' Rename column names
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param .data a dataframe with columns to rename
#' @param to_be_renamed_list  a list of the new column names assigned
#'        to the old column names
#'        ie. list('old column names' = 'new column names')
#'
#' @return original dataframe containing new columns of the renamed columns
#' with tidyverse code attached
#' @seealso \code{\link{code}}
#'
#' @examples
#' renamed <- renameVars(iris,
#'     to_be_renamed_list = list(Species = "Type", Petal.Width = "P.W"))
#' cat(code(renamed))
#' head(renamed)
#'
#' @author Owen Jin
#' @export
renameVars <- function(.data, to_be_renamed_list) {
    mc <- match.call()
    dataname <- mc$.data

    # paste together the variables to be renamed into one string
    vnames <- names(to_be_renamed_list)
    to_be_renamed_list <- create_varname(to_be_renamed_list)
    to_be_renamed <- stringr::str_c(to_be_renamed_list, "=", vnames,
        collapse = ", "
    )

    if (is_survey(.data) && !inherits(.data, "tbl_svy")) {
        .data <- srvyr::as_survey(.data)
        dataname <- glue::glue("{dataname} %>% srvyr::as_survey()")
    }

    exp <- ~.DATA %>%
        dplyr::rename(.RENAME)
    exp <- replaceVars(exp,
        .RENAME = to_be_renamed,
        .DATA = dataname
    )

    interpolate(exp)
}
