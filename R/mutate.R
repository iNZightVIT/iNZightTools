#' Replace NAs with specified values
#' @name replace_na
#' @importFrom tidyr replace_na
#' @export
NULL

#' Make \code{\link[tidyr]{replace_na}} also work for factors.
#' @inheritParams tidyr::replace_na
#' @param replace If \code{data} is a factor, the default value
#'        will be \code{"<NA>"}.
#' @importFrom tidyr replace_na
#' @rdname replace_na
#' @export
replace_na.factor <- function(data, replace = "<NA>", ...) {
    as.character(data) |>
        tidyr::replace_na(replace, ...) |>
        factor(levels = c(
            ifelse(any(is.na(data)), replace, ""),
            levels(data)
        ))
}


#' Combine categorical variables into one
#'
#' Combine specified categorical variables by concatenating
#' their values into one character, and returns the result
#' along with tidyverse code used to generate it.
#'
#' @param data a dataframe with the columns to be combined
#' @param vars  a character vector of the categorical variables to be combined
#' @param sep a character string to separate the levels
#' @param name a name for the new variable
#' @param keep_empty logical, if \code{FALSE} empty level combinations
#'        are removed from the factor
#' @param keep_na logical, if \code{TRUE} the \code{<NA>} in the factors or
#'        \code{NA} in the characters will be replaced with \code{"<NA>"};
#'        otherwise, the resulting entries will return \code{<NA>}
#'
#' @return original dataframe containing a new column of the renamed
#'         categorical variable with tidyverse code attached
#' @rdname combine_cat
#' @examples
#' combined <- combine_cat(warpbreaks, vars = c("wool", "tension"), sep = "_")
#' cat(code(combined))
#' head(combined)
#'
#' @author Owen Jin, Stephen Su
#' @export
combine_cat <- function(data, vars, sep = ":", name = NULL,
                        keep_empty = FALSE, keep_na = TRUE) {
    expr <- rlang::enexpr(data)
    if (is.null(name)) {
        name <- paste(vars, collapse = sep)
    }
    if (keep_na) {
        vars <- rlang::parse_exprs(sprintf("replace_na(%s)", vars))
    } else {
        vars <- rlang::syms(vars)
    }
    if (is_survey(data)) {
        expr <- coerce_tbl_svy(expr, data)
        expr <- rlang::expr(!!expr %>% srvyr::mutate(
            !!name := forcats::fct_cross(!!!vars,
                sep = !!sep, keep_empty = !!keep_empty
            )
        ))
    } else {
        expr <- rlang::expr(!!expr %>% dplyr::mutate(
            !!name := forcats::fct_cross(!!!vars,
                sep = !!sep, keep_empty = !!keep_empty
            )
        ))
    }
    eval_code(expr)
}
