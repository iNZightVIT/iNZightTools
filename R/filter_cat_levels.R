#' Filter data by levels of a categorical variables
#'
#' Filter a dataframe by some levels of one categorical variable
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param .data a dataframe to filter
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

    exp <- ~.DATA %>%
        dplyr::filter(.VARNAME %in% .LEVELS) %>%
        dplyr::mutate(.VARNAME = factor(.VARNAME, levels = .LEVELS))
    exp <- replaceVars(exp,
        .VARNAME = var,
        .LEVELS = as.list(levels),
        .DATA = dataname
    )

    interpolate(exp)
}