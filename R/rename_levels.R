#' Rename the levels of a categorical variable
#'
#' Rename the levels of a categorical variables, and returns the result
#' along with tidyverse code used to generate it.
#'
#' @param .data a dataframe with the column to be renamed
#' @param var  a character of the categorical variable to rename
#' @param to_be_renamed a list of the old level name assigned
#'        to the new level name;
#'        i.e., `list('new level name' = 'old level name')`
#' @param name a name for the new variable
#'
#' @return original dataframe containing a new column of the renamed categorical
#'         variable with tidyverse code attached
#'
#' @seealso \code{\link{code}}
#' @examples
#' renamed <- renameLevels(iris, var = "Species",
#'     to_be_renamed = list(set = "setosa", ver = "versicolor"))
#' cat(code(renamed))
#' head(renamed)
#'
#' @author Owen Jin
#' @export
renameLevels <- function(.data, var, to_be_renamed,
                         name = sprintf("%s.rename", var)) {
    mc <- match.call()
    dataname <- mc$.data

    # paste together the new and old factor levels names to be renamed
    to_be_renamed <- str_c(
        names(to_be_renamed), ' = "', to_be_renamed, '"',
        collapse = ", "
    )

    fmla <- ".NAME = forcats::fct_recode(.VNAME, .RENAME)"
    fmla <- gsub(".VNAME",
        ifelse(is_survey(.data), ".VARNAME", ".DATA$.VARNAME"),
        fmla
    )

    if (is_survey(.data)) {
        exp <- ~.DATA %>% update(.FMLA)
    } else {
        exp <- ~.DATA %>% tibble::add_column(.FMLA, .after = ".VARNAME")
    }
    exp <- replaceVars(exp,
        .FMLA = fmla,
        .DATA = dataname ,
        .RENAME = to_be_renamed,
        .VARNAME = var,
        .NAME = name
    )

    interpolate(exp)
}
