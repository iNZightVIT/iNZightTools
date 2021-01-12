#' Filter data by levels of a categorical variables
#'
#' Filter a dataframe by some levels of one categorical variable
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param .data a dataframe or survey design object to filter
#' @param var character of the column in \code{.data} to filter by
#' @param levels a character vector of levels in \code{var} to filter by
#'
#' @return filtered dataframe with tidyverse code attached
#' @seealso \code{\link{code}}
#' @examples
#' filtered <- filterLevels(iris, var = "Species",
#'     levels = c("versicolor", "virginica"))
#' cat(code(filtered))
#' head(filtered)
#'
#' @author Owen Jin
#' @export
filterLevels <- function(.data, var, levels) {
    mc <- match.call()
    dataname <- mc$.data

    is_survey <- inherits(.data, "survey.design")
    if (is_survey) {
        .data <- srvyr::as_survey_design(.data)
        dataname <- glue::glue("srvyr::as_survey_design({dataname})")
    }

    operator <- if (length(levels) == 1) " == " else " %in% "

    exp <- ~.DATA %>%
        dplyr::filter(.VARNAME.OP.LEVELS)
    # remove levels from factor (and ensure correct order)
    if (length(levels) > 1)
        exp <- paste(paste(exp, collapse = ""),
            " %>% droplevels()")
            # "%>% dplyr::mutate(.VARNAME = factor(.VARNAME, levels = .LEVELS))")

    exp <- replaceVars(exp,
        .VARNAME = var,
        .OP = operator,
        .DATA = dataname
    )

    interpolate(exp, .LEVELS = levels)
}
