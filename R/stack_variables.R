#' Stack variables
#'
#' Collapse columns by converting from a wide to a long format
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param .data a dataframe to stack
#' @param vars  a character vector of variables to stack
#' @param key name of the new column for the stacked variables.
#'        "stack.variable" by default
#' @param value name of the new column for the stacked values of the stacked.
#'        "stack.value" by default
#'
#' @return stacked dataframe with tidyverse code attached
#' @seealso \code{\link{code}}
#' @examples
#' stacked <- stackVars(iris, vars = c("Species", "Sepal.Width"),
#'     key = "Variable", value = "Value")
#' cat(code(stacked))
#' head(stacked)
#'
#' @author Owen Jin
#' @export
stackVars <- function(.data, vars,
                      key = "stack.variable",
                      value = "stack.value") {

    if (is_survey(.data)) stop('Cannot stack data in a survey design')

    mc <- match.call()
    dataname <- mc$.data

    # paste together the variables to be stacked into a string
    to_be_stacked <- str_c(vars, collapse = ", ")

    exp <- ~.DATA %>%
        tidyr::gather(key = .KEY, value = .VALUE, .VARNAMES)
    exp <- replaceVars(exp,
        .VARNAMES = to_be_stacked,
        .DATA = dataname
    )

    interpolate(exp,
        .KEY = key,
        .VALUE = value
    )
}
