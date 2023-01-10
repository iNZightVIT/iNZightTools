mutate_expr <- function(expr, vars_expr, data) {
    if (is_survey(data)) {
        expr <- coerce_tbl_svy(expr, data)
        rlang::expr(!!expr %>% srvyr::mutate(!!!vars_expr))
    } else {
        rlang::expr(!!expr %>% dplyr::mutate(!!!vars_expr))
    }
}


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
#' @return original dataframe containing new columns of the renamed
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
#' @rdname mutate_as_cat
#' @seealso \code{\link{code}}
#' @examples
#' converted <- mutate_as_cat(iris, vars = c("Petal.Width"))
#' cat(code(converted))
#' head(converted)
#'
#' @author Stephen Su
#' @export
mutate_as_cat <- function(data, vars, names = NULL) {
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
#' @rdname mutate_as_datetime
#' @seealso \code{\link{code}}
#' @export
#' @author Stephen Su
mutate_as_datetime <- function(data, vars, names = NULL, tz = "") {
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
#' @rdname mutate_as_date
#' @seealso \code{\link{code}}
#' @export
#' @author Stephen Su
mutate_as_date <- function(data, vars, names = NULL) {
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
#' created <- mutate_new(
#'     data = iris,
#'     vars = "Sepal.Length_less_Sepal.Width",
#'     "Sepal.Length - Sepal.Width"
#' )
#' cat(code(created))
#' head(created)
#' @rdname mutate_new
#' @author Stephen Su
#' @export
mutate_new <- function(data, vars = ".new_var", vars_expr = NULL) {
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
#' @rdname mutate_transform
#' @seealso \code{\link{code}}
#' @examples
#' transformed <- mutate_transform(iris,
#'     var = "Petal.Length",
#'     fn = "log"
#' )
#' cat(code(transformed))
#' head(transformed)
#'
#' @author Stephen Su
#' @export
mutate_transform <- function(data, vars, fn, names = NULL) {
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
#' @param from a character vector the variable names as-is
#' @param to a character vector the variable names to-be
#' @return original dataframe containing new columns of the renamed columns
#'        with tidyverse code attached
#' @rdname rename_vars
#' @seealso \code{\link{code}}
#' @examples
#' renamed <- rename_vars(iris,
#'     from = names(iris),
#'     to = tolower(gsub("\\.", "_", names(iris)))
#' )
#' cat(code(renamed))
#' head(renamed)
#'
#' @author Stephen Su
#' @export
rename_vars <- function(data, from, to) {
    expr <- rlang::enexpr(data)
    if (length(from) != length(to)) {
        rlang::abort("`from` and `to` must have same length.")
    }
    vars_expr <- purrr::map(from, rlang::sym) |>
        rlang::set_names(to)
    if (is_survey(data)) {
        expr <- coerce_tbl_svy(expr, data)
        expr <- rlang::expr(!!expr %>% srvyr::rename(!!!vars_expr))
    } else {
        expr <- rlang::expr(!!expr %>% dplyr::rename(!!!vars_expr))
    }
    eval_code(expr)
}


#' Collapse data by values of a categorical variable
#'
#' Collapse values in a categorical variable into less levels of values
#'
#' @param data a dataframe to collapse
#' @param var  a string of the name of the categorical variable to collapse
#' @param collapsed  a named list: the values of the categorical variable
#'        matching each element in the list will be changed to the name of that
#'        element in the list
#' @param name a name for the new variable
#' @return the original dataframe containing a new column of the
#'         collapsed variable with tidyverse code attached
#' @rdname collapse_cat
#' @seealso \code{\link{code}}
#'
#' @examples
#' collapsed <- collapse_cat(iris,
#'     var = "Species",
#'     list(S = "setosa", V = c("versicolor", "virginica"))
#' )
#' cat(code(collapsed))
#' head(collapsed)
#'
#' @author Stephen Su
#' @export
collapse_cat <- function(data, var, collapsed = NULL, name = NULL) {
    expr <- rlang::enexpr(data)
    if (length(var) > 1) {
        var <- var[1]
        rlang::warn(sprintf(
            "Please specify one variable, setting `var = %s`",
            var
        ))
    }
    if (is.null(name)) {
        name <- sprintf("%s.coll", var)
    }
    fctr <- purrr::map(collapsed, function(x) {
        if (length(x) > 1) {
            ## Parse the elements into expressions in the form of c(...)
            purrr::map_chr(x, function(x) sprintf("\"%s\"", x)) |>
                (\(.) sprintf("c(%s)", paste(., collapse = ", ")))() |>
                rlang::parse_expr()
        } else {
            rlang::parse_expr(sprintf("\"%s\"", x))
        }
    }) |> rlang::set_names(names(collapsed))
    vars_expr <- rlang::list2(
        !!name := rlang::expr(forcats::fct_collapse(!!rlang::sym(var), !!!fctr))
    )
    expr <- mutate_expr(expr, vars_expr, data)
    eval_code(expr)
}
