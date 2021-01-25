#' Convert missing values to categorical variables
#'
#' Turn <NA>'s into a "missing" character;
#' hence numeric variables will be converted to categorical variables
#' with any numeric values will be converted to "observed",
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param .data a dataframe with the columns to convert
#'        its missing values into categorical
#' @param vars  a character vector of the variables in \code{.data}
#'        for conversion of missing values to categorical
#' @param names a vector of names for the new variables
#' @return original dataframe containing new columns of the converted variables
#'        for the missing values
#'        with tidyverse code attached
#' @seealso \code{\link{code}}
#' @examples
#' missing <- missingToCat(iris, vars = c("Species", "Sepal.Length"))
#' cat(code(missing))
#' head(missing)
#'
#' @author Owen Jin
#' @export
missingToCat <- function(.data, vars, names = paste0(vars, "_miss")) {
    mc <- match.call()
    dataname <- mc$.data

    .d <- if (is_survey(.data)) .data$variables else .data
    numCols <- sapply(.d[, vars], is.numeric)

    formulae <- sapply(seq_along(vars),
        function(i) {
            if (numCols[i]) {
                ## it's a number
                fmla <- ".NAME = factor(ifelse(is.na(.VAR), \"missing\", \"observed\"))"
            }
            else {
                fmla <- ".NAME = forcats::fct_explicit_na(.VAR, na_level = \"missing\")"
            }
            fmla <- gsub(".VAR", ifelse(is_survey(.data), ".VARNAME", ".DATA$.VARNAME"), fmla)
            if (!is_survey(.data)) {
                fmla <- sprintf("tibble::add_column(%s, .after = \".VARNAME\")", fmla)
            }
            fmla <- gsub(".VARNAME", vars[i], fmla)
            fmla <- gsub(".NAME", names[i], fmla)
            if (is_survey(.data)) return(fmla)
            eval(parse(text = sprintf("~%s", fmla)))
        }
    )

    if (is_survey(.data)) {
        exp <- ~.DATA %>% update(.FMLA)
        formula <- paste(formulae, collapse = ", ")
        exp <- replaceVars(exp, .FMLA = formula)
    } else {
        exp <- pasteFormulae(c(list(~.DATA), as.list(formulae)))
    }

    exp <- replaceVars(exp, .DATA = dataname)

    interpolate(exp)
}
