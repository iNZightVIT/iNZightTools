#' Reorder a categorical
#'
#' Reorder the factors of a categorical variable either manually or frequency
#'
#' @param .data a dataframe to reorder
#' @param var a categorical variable to reorder
#' @param  new_levels a character vector of the new factor order.
#'         Only specify if \code{freq} = FALSE
#' @param freq logical, If \code{freq} = \code{FALSE} (default),
#'        will manually reorder using \code{new_levels}.
#'        If \code{freq} = \code{TRUE}, will reorder
#'        based of descending frequency of the factor levels
#' @param name name for the new variable
#'
#' @return original dataframe containing a new column of the reordered
#'         categorical variable with tidyverse code attached
#' @seealso \code{\link{code}}
#' @examples
#' reordered <- reorderLevels(iris, var = "Species",
#'     new_levels = c("versicolor", "virginica", "setosa"))
#' cat(code(reordered))
#' head(reordered)
#'
#' @author Owen Jin
#' @export
reorderLevels <- function(.data, var,
                          new_levels = NULL,
                          freq = FALSE,
                          name = sprintf("%s.reord", var)) {
    mc <- match.call()
    dataname <- mc$.data

    # fix the name
    i <- 1
    after <- var
    while (name %in% colnames(.data)) {
        i <- i + 1
        after <- name
        name <- sprintf("%s.reord%i", var, i)
    }


    fmla <- ifelse(freq,
        ".NEWNAME = forcats::fct_infreq(.VNAME)",
        ".NEWNAME = factor(.VNAME, levels = .NEWLEVELS)"
    )
    fmla <- gsub(".VNAME",
        ifelse(is_survey(.data), ".VARNAME", ".DATA$.VARNAME"),
        fmla
    )

    if (is_survey(.data)) {
        exp <- ~.DATA %>% update(.FMLA)
    } else {
        exp <- ~.DATA %>% tibble::add_column(.FMLA, .after = ".AFTER")
    }

    exp <- replaceVars(exp,
        .FMLA = fmla,
        .NEWNAME = name,
        .AFTER = after,
        .VARNAME = var,
        .DATA = dataname,
        .NEWLEVELS = new_levels
    )

    interpolate(exp)
}
