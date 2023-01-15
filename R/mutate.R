mutate_expr <- function(expr, vars_expr, data) {
    if (is_survey(data)) {
        expr <- coerce_tbl_svy(expr, data)
        rlang::expr(!!expr %>% srvyr::mutate(!!!vars_expr))
    } else {
        rlang::expr(!!expr %>% dplyr::mutate(!!!vars_expr))
    }
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
#' @return original dataframe containing new columns of the renamed
#'         categorical variable with tidyverse code attached
#' @rdname combine_cat
#' @importFrom forcats fct_explicit_na
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
        vars <- rlang::parse_exprs(sprintf("fct_explicit_na(%s)", vars))
    } else {
        vars <- rlang::syms(vars)
    }
    vars_expr <- rlang::list2(
        !!name := rlang::expr(forcats::fct_cross(
            !!!vars,
            sep = !!sep,
            keep_empty = !!keep_empty
        ))
    )
    expr <- mutate_expr(expr, vars_expr, data)
    eval_code(expr)
}


#' Convert variables to categorical variables
#'
#' Convert specified variables into factors
#'
#' @param data a dataframe with the categorical column to convert
#' @param vars  a character vector of column names to convert
#' @param names a character vector of names for the created variables
#' @return original dataframe containing new columns of the
#'         converted variables with tidyverse code attached
#' @rdname convert_to_cat
#' @seealso \code{\link{code}}
#' @examples
#' converted <- convert_to_cat(iris, vars = c("Petal.Width"))
#' cat(code(converted))
#' head(converted)
#'
#' @author Stephen Su
#' @export
convert_to_cat <- function(data, vars, names = NULL) {
    expr <- rlang::enexpr(data)
    if (is.null(names)) {
        names <- sprintf("%s.cat", vars)
    }
    vars_expr <- purrr::map(vars, function(x) {
        rlang::expr(as.factor(!!rlang::sym(x)))
    }) |> rlang::set_names(names)
    expr <- mutate_expr(expr, vars_expr, data)
    eval_code(expr)
}


#' Convert variables to date-time
#'
#' @param data a dataframe with the variables to convert
#' @param vars a character vector of column names to convert
#' @param names a character vector of names for the created variables
#' @param tz a time zone name (default: local time zone). See
#'        \code{\link[base]{OlsonNames}}
#' @return original dataframe containing new columns of the
#'         converted variables with tidyverse code attached
#' @rdname convert_to_datetime
#' @seealso \code{\link{code}}
#' @export
#' @author Stephen Su
convert_to_datetime <- function(data, vars, names = NULL, tz = "") {
    expr <- rlang::enexpr(data)
    if (is.null(names)) {
        names <- sprintf("%s.datetime", vars)
    }
    vars_expr <- purrr::map(vars, function(x) {
        rlang::expr(lubridate::as_datetime(!!rlang::sym(x), tz = !!tz))
    }) |> rlang::set_names(names)
    expr <- mutate_expr(expr, vars_expr, data)
    eval_code(expr)
}


#' Convert variables to dates
#'
#' @param data a dataframe with the variables to convert
#' @param vars a character vector of column names to convert
#' @param names a character vector of names for the created variables
#' @return original dataframe containing new columns of the
#'         converted variables with tidyverse code attached
#' @rdname convert_to_date
#' @seealso \code{\link{code}}
#' @export
#' @author Stephen Su
convert_to_date <- function(data, vars, names = NULL) {
    expr <- rlang::enexpr(data)
    if (is.null(names)) {
        names <- sprintf("%s.date", vars)
    }
    vars_expr <- purrr::map(vars, function(x) {
        rlang::expr(lubridate::as_date(!!rlang::sym(x)))
    }) |> rlang::set_names(names)
    expr <- mutate_expr(expr, vars_expr, data)
    eval_code(expr)
}


#' Create new variables
#'
#' Create new variables by using valid R expressions and returns
#' the result along with tidyverse code used to generate it.
#'
#' @param data a dataframe to which to add new variables to
#' @param vars a character of the new variable names
#' @param vars_expr a character of valid R expressions which can
#'        generate vectors of values
#' @return original dataframe containing the new columns
#'         created from \code{vars_expr}
#' with tidyverse code attached
#' @seealso \code{\link{code}}
#' @examples
#' created <- create_vars(
#'     data = iris,
#'     vars = "Sepal.Length_less_Sepal.Width",
#'     "Sepal.Length - Sepal.Width"
#' )
#' cat(code(created))
#' head(created)
#' @rdname create_vars
#' @author Stephen Su
#' @export
create_vars <- function(data, vars = ".new_var", vars_expr = NULL) {
    expr <- rlang::enexpr(data)
    vars_expr <- purrr::map(vars_expr, rlang::parse_expr) |>
        rlang::set_names(vars)
    expr <- mutate_expr(expr, vars_expr, data)
    eval_code(expr)
}


#' Delete variables from a dataset
#'
#' @title Delete variables
#' @param data dataset
#' @param vars variable names to delete
#' @return dataset without chosen variables
#' @author Stephen Su
#' @rdname delete_vars
#' @export
delete_vars <- function(data, vars = NULL) {
    expr <- rlang::enexpr(data)
    vars_expr <- replicate(length(vars), NULL) |>
        rlang::set_names(vars)
    expr <- mutate_expr(expr, vars_expr, data)
    eval_code(expr)
}


#' Transform data of numeric variables
#'
#' Transform the values of numeric variables by applying
#' a mathematical function
#' @param data a dataframe with the variables to transform
#' @param vars  a character of the numeric variables in \code{data} to transform
#' @param fn  the name (a string) of a valid R function
#' @param names the names of the new variables
#'
#' @return the original dataframe containing the new columns of the transformed
#'         variable with tidyverse code attached
#' @rdname transform_vars
#' @seealso \code{\link{code}}
#' @examples
#' transformed <- transform_vars(iris,
#'     var = "Petal.Length",
#'     fn = "log"
#' )
#' cat(code(transformed))
#' head(transformed)
#'
#' @author Stephen Su
#' @export
transform_vars <- function(data, vars, fn, names = NULL) {
    expr <- rlang::enexpr(data)
    if (is.null(names)) {
        names <- sprintf("%s.%s", fn, vars)
    }
    vars_expr <- purrr::map(vars, function(x) {
        rlang::expr((!!rlang::parse_expr(fn))(!!rlang::sym(x)))
    }) |> rlang::set_names(names)
    expr <- mutate_expr(expr, vars_expr, data)
    eval_code(expr)
}


#' Rename column names
#'
#' Rename columns of a dataset with desired names
#'
#' @param data a dataframe with columns to rename
#' @param tobe_asis  a named list of the old column names assigned
#'        to the new column names
#'        ie. list('new column names' = 'old column names')
#' @return original dataframe containing new columns of the renamed columns
#'        with tidyverse code attached
#' @rdname rename_vars
#' @seealso \code{\link{code}}
#' @examples
#' renamed <- rename_vars(iris, list(
#'     sepal_length = "Sepal.Length",
#'     sepal_width = "Sepal.Width",
#'     petal_length = "Petal.Length",
#'     petal_width = "Petal.Width"
#' ))
#' cat(code(renamed))
#' head(renamed)
#'
#' @author Stephen Su
#' @export
rename_vars <- function(data, tobe_asis) {
    expr <- rlang::enexpr(data)
    if (is_survey(data)) {
        expr <- coerce_tbl_svy(expr, data)
        expr <- rlang::expr(!!expr %>% srvyr::rename(!!!tobe_asis))
    } else {
        expr <- rlang::expr(!!expr %>% dplyr::rename(!!!tobe_asis))
    }
    eval_code(expr)
}


#' Collapse data by values of a categorical variable
#'
#' Collapse values in a categorical variable into one defined level
#'
#' @param data a dataframe to collapse
#' @param var  a string of the name of the categorical variable to collapse
#' @param levels  a character vector of the levels to be collapsed
#' @param new_level a string for the new level
#' @param name a name for the new variable
#' @return the original dataframe containing a new column of the
#'         collapsed variable with tidyverse code attached
#' @rdname collapse_cat
#' @seealso \code{\link{code}}
#'
#' @examples
#' collapsed <- collapse_cat(iris,
#'     var = "Species",
#'     c("versicolor", "virginica"),
#'     new_level = "V"
#' )
#' cat(code(collapsed))
#' tail(collapsed)
#'
#' @author Stephen Su
#' @export
collapse_cat <- function(data, var, levels, new_level, name = NULL) {
    expr <- rlang::enexpr(data)
    if (is.null(name)) {
        name <- sprintf("%s.coll", var)
    }
    fctr <- rlang::list2(!!new_level := rlang::enexpr(levels))
    vars_expr <- rlang::list2(
        !!name := rlang::expr(forcats::fct_collapse(!!rlang::sym(var), !!!fctr))
    )
    expr <- mutate_expr(expr, vars_expr, data)
    eval_code(expr)
}
