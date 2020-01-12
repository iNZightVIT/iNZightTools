#' Collapse data by values of a categorical variable
#'
#' Collapse several values in a categorical variable into one level
#'
#' @param .data a dataframe to collapse
#' @param var  a character of the name of the categorical variable to collapse
#' @param levels  a character vector of the levels to be collapsed
#' @param collapse name of the newly created level
#' @param name a name for the new variable
#' @return the original dataframe containing a new column of the
#'         collapsed variable with tidyverse code attached
#' @seealso \code{\link{code}}
#'
#' @examples
#' collapsed <- collapseLevels(iris, var = "Species",
#'     levels = c("setosa", "virginica"))
#' cat(code(collapsed))
#' head(collapsed)
#'
#' @author Owen Jin
#' @export
collapseLevels <- function(.data, var, levels,
                           collapse = paste(levels, collapse = "_"),
                           name = sprintf("%s.coll", var)) {
    mc <- match.call()
    dataname <- mc$.data

    exp <- ~.DATA %>%
        tibble::add_column(
            .NAME = forcats::fct_collapse(
                .DATA$.VARNAME,
                .COLLAPSENAME = .LEVELS
            ),
            .after = ".VARNAME")

    # cannot start a variable name with a number
    if (grepl("^[0-9]", collapse))
        collapse <- sprintf("`%s`", collapse)

    exp <- replaceVars(exp,
        .VARNAME = var,
        .COLLAPSENAME = collapse,
        .LEVELS = levels,
        .DATA = dataname,
        .NAME = name
    )

    interpolate(exp)
}
