#' Transform data of a numeric variable
#'
#' Transform the values of a numeric variable by applying
#' a mathematical function
#' @param .data a dataframe with the variables to transform
#' @param var  a character of the numeric variable in \code{.data} to transform
#' @param transformation  a name of a valid mathematical function that can
#'        be applied to numeric values, eg. "log", "exp", "sqrt".
#'        For squaring, use "square"; for inverting, use "reciprocal"
#' @param name the name of the new variable
#'
#' @return the original dataframe containing a new column of the transformed
#'         variable with tidyverse code attached
#' @seealso \code{\link{code}}
#' @examples
#' transformed <- transformVar(iris, var = "Petal.Length",
#'     transformation = "log")
#' cat(code(transformed))
#' head(transformed)
#'
#' @author Owen Jin
#' @export
transformVar <- function(.data, var, transformation,
                         name = sprintf("%s.%s", transformation, var)) {
    mc <- match.call()
    dataname <- mc$.data

    funexp <- switch(transformation,
        reciprocal = "1 / .DATA$.VARNAME",
        square = ".DATA$.VARNAME^2",
        ".FUNNAME(.DATA$.VARNAME)"
    )

    exp <- ~.DATA %>%
        tibble::add_column(.NAME = .FUNEXP, .after = ".VARNAME")

    exp <- replaceVars(exp,
        .FUNEXP = funexp,
        .DATA = dataname,
        .NAME = name,
        .VARNAME = var,
        .FUNNAME = transformation
    )

    interpolate(exp)
}
