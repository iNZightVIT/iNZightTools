mutate_expr <- function(expr, vars_expr, data) {
    if (is_survey(data)) {
        expr <- coerce_tbl_svy(expr, data)
        rlang::expr(!!expr %>% srvyr::mutate(!!!vars_expr))
    } else {
        rlang::expr(!!expr %>% dplyr::mutate(!!!vars_expr))
    }
}


mutate_expr_i <- function(expr, vars_expr, data, ...) {
    purrr::map(seq_along(vars_expr), function(i) {
        vars_expr_i <- rlang::list2(
            !!names(vars_expr)[i] := vars_expr[[i]],
            !!!purrr::map(list(...), function(x) rlang::sym(x[i]))
        )
        expr <<- mutate_expr(expr, vars_expr_i, data)
    })
    expr
}


#' Combine variables into one categorical variable
#'
#' Combine chosen variables of any class by concatenating
#' them into one factor variable, and returns the result
#' along with tidyverse code used to generate it.
#'
#' @param data a dataframe with the columns to be combined
#' @param vars  a character vector of the variables to be combined
#' @param sep a character string to separate the levels
#' @param name a name for the new variable
#' @param keep_empty logical, if \code{FALSE} empty level combinations
#'        are removed from the factor
#' @param keep_na logical, if \code{TRUE} the \code{<NA>} in the factors or
#'        \code{NA} in the characters will turn in a level \code{"(Missing)"};
#'        otherwise, the resulting entries will return \code{<NA>}
#'
#' @return original dataframe containing new columns of the new
#'         categorical variable with tidyverse code attached
#' @rdname combine_vars
#' @examples
#' combined <- combine_vars(warpbreaks, vars = c("wool", "tension"), sep = "_")
#' cat(code(combined))
#' head(combined)
#'
#' @author Owen Jin, Zhaoming Su
#' @export
combine_vars <- function(data, vars, sep = ":", name = NULL,
                         keep_empty = FALSE, keep_na = TRUE) {
    expr <- rlang::enexpr(data)
    n_max <- getOption("inzighttools.max_levels", 100)
    .data <- if (is_survey(data)) data$variables else data
    n_lvl <- purrr::map_dbl(vars, function(x) {
        length(unique(.data[[x]]))
    }) |> prod()
    if (n_lvl > n_max) {
        rlang::abort(sprintf(paste(
            "Resulting factor has more levels than the allowed limit (%s).",
            "Try using `collapse_cat()` to reduce the number of levels."
        ), n_max))
    }
    if (is.null(name)) {
        name <- paste(vars, collapse = sep)
    }
    vars_not_cat <- purrr::map_lgl(vars, function(x) {
        !inherits(.data[[x]], c("factor", "character"))
    })
    vars[vars_not_cat] <- sprintf("as.factor(%s)", vars[vars_not_cat])
    if (keep_na) {
        vars <- sprintf("forcats::fct_na_value_to_level(%s, \"(Missing)\")", vars) |>
            rlang::parse_exprs()
    } else {
        vars <- rlang::parse_exprs(vars)
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
#' @author Zhaoming Su
#' @export
convert_to_cat <- function(data, vars, names = NULL) {
    expr <- rlang::enexpr(data)
    if (is.null(names)) {
        names <- sprintf("%s.cat", vars)
    }
    vars_expr <- purrr::map(vars, function(x) {
        rlang::expr(as.factor(!!rlang::sym(x)))
    }) |> rlang::set_names(names)
    expr <- mutate_expr_i(expr, vars_expr, data, .after = vars)
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
#' @author Zhaoming Su
#' @export
create_vars <- function(data, vars = ".new_var", vars_expr = NULL) {
    expr <- rlang::enexpr(data)
    vars_expr <- rlang::parse_exprs(vars_expr) |>
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
#' @author Zhaoming Su
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
#' @author Zhaoming Su
#' @export
transform_vars <- function(data, vars, fn, names = NULL) {
    expr <- rlang::enexpr(data)
    if (is.null(names)) {
        names <- sprintf("%s.%s", fn, vars)
    }
    vars_expr <- purrr::map(vars, function(x) {
        rlang::expr((!!rlang::parse_expr(fn))(!!rlang::sym(x)))
    }) |> rlang::set_names(names)
    expr <- mutate_expr_i(expr, vars_expr, data, .after = vars)
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
#' @author Zhaoming Su
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
#' @author Zhaoming Su
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
    expr <- mutate_expr_i(expr, vars_expr, data, .after = var)
    eval_code(expr)
}


#' Standardize the data of a numeric variable
#'
#' Centre then divide by the standard error of the values in a numeric variable
#'
#' @param data a dataframe with the columns to standardize
#' @param vars  a character vector of the numeric variables in \code{data}
#'        to standardize
#' @param names names for the created variables
#'
#' @return the original dataframe containing new columns of the standardized
#'         variables with tidyverse code attached
#' @rdname standardize_vars
#' @seealso \code{\link{code}}
#' @examples
#' standardized <- standardize_vars(iris, var = c("Sepal.Width", "Petal.Width"))
#' cat(code(standardized))
#' head(standardized)
#'
#' @author Zhaoming Su
#' @export
standardize_vars <- function(data, vars, names = NULL) {
    expr <- rlang::enexpr(data)
    if (is.null(names)) {
        names <- sprintf("%s.std", vars)
    }
    vars_expr <- purrr::map(vars, function(x) {
        if (is_survey(data)) {
            x <- rlang::sym(x)
            rlang::expr((!!x - svymean(~ !!x, !!expr, na.rm = TRUE)) /
                sqrt(svyvar(~ !!x, !!expr, na.rm = TRUE)))
        } else {
            rlang::expr(scale(!!rlang::sym(x))[, 1])
        }
    }) |> rlang::set_names(names)
    expr <- mutate_expr_i(expr, vars_expr, data, .after = vars)
    eval_code(expr)
}


#' Rank the data of numeric variables
#'
#' Rank the values of numeric variables, for example, in descending order,
#' and then returns the result along with tidyverse code used to generate it.
#' See \code{\link[dplyr]{row_number}} and \code{\link[dplyr]{percent_rank}}.
#'
#' @param data a dataframe with the variables to rank
#' @param vars  a character vector of numeric variables in \code{data}
#'        to rank
#' @param rank_type either \code{"min"}, \code{"dense"} or \code{"percent"},
#'        see \code{\link[dplyr]{row_number}}, \code{\link[dplyr]{percent_rank}}
#'
#' @return the original dataframe containing new columns with the ranks of the
#'         variables in \code{vars} with tidyverse code attached
#' @rdname rank_vars
#' @seealso \code{\link{code}}
#' @examples
#' ranked <- rank_vars(iris, vars = c("Sepal.Length", "Petal.Length"))
#' cat(code(ranked))
#' head(ranked)
#'
#' @author Zhaoming Su
#' @export
rank_vars <- function(data, vars, rank_type = c("min", "dense", "percent")) {
    rank_type <- rlang::arg_match(rank_type)
    expr <- rlang::enexpr(data)
    rank_fn <- sprintf("dplyr::%s_rank", rank_type)
    vars_expr <- purrr::map(vars, function(x) {
        rlang::expr((!!rlang::parse_expr(rank_fn))(!!rlang::sym(x)))
    }) |> rlang::set_names(sprintf("%s.%s_rank", vars, rank_type))
    expr <- mutate_expr_i(expr, vars_expr, data, .after = vars)
    eval_code(expr)
}


#' Rename the levels of a categorical variable
#'
#' Rename the levels of a categorical variables, and returns the result
#' along with tidyverse code used to generate it.
#'
#' @param data a dataframe with the column to be renamed
#' @param var  a character of the categorical variable to rename
#' @param tobe_asis  a named list of the old level names assigned
#'        to the new level names
#'        ie. list('new level names' = 'old level names')
#' @param name a name for the new variable
#'
#' @return original dataframe containing a new column of the renamed categorical
#'         variable with tidyverse code attached
#' @rdname rename_levels
#'
#' @seealso \code{\link{code}}
#' @examples
#' renamed <- rename_levels(iris,
#'     var = "Species",
#'     tobe_asis = list(set = "setosa", ver = "versicolor")
#' )
#' cat(code(renamed))
#' head(renamed)
#'
#' @author Zhaoming Su
#' @export
rename_levels <- function(data, var, tobe_asis, name = NULL) {
    expr <- rlang::enexpr(data)
    if (is.null(name)) {
        name <- sprintf("%s.renamed", var)
    }
    vars_expr <- rlang::list2(!!name := rlang::expr(
        forcats::fct_recode(!!rlang::sym(var), !!!tobe_asis)
    ))
    expr <- mutate_expr_i(expr, vars_expr, data, .after = var)
    eval_code(expr)
}


#' Reorder the levels of a categorical variable
#'
#' Reorder the levels of a categorical variable either manually or automatically
#'
#' @param data a dataframe to reorder
#' @param var a categorical variable to reorder
#' @param new_levels a character vector of the new factor order;
#'        overrides \code{auto} if not \code{NULL}
#' @param auto only meaningful if \code{new_levels} is \code{NULL}: the method
#'        to auto-reorder the levels, see \code{\link[forcats]{fct_inorder}}
#' @param name name for the new variable
#'
#' @return original dataframe containing a new column of the reordered
#'         categorical variable with tidyverse code attached
#' @rdname reorder_levels
#' @seealso \code{\link{code}}
#' @examples
#' reordered <- reorder_levels(iris,
#'     var = "Species",
#'     new_levels = c("versicolor", "virginica", "setosa")
#' )
#' cat(code(reordered))
#' head(reordered)
#'
#' reordered <- reorder_levels(iris,
#'     var = "Species",
#'     auto = "freq"
#' )
#' cat(code(reordered))
#' head(reordered)
#'
#' @author Zhaoming Su
#' @export
reorder_levels <- function(data, var, new_levels = NULL,
                           auto = c("freq", "order", "seq"),
                           name = NULL) {
    expr <- rlang::enexpr(data)
    new_lvls <- rlang::enexpr(new_levels)
    if (is.null(new_levels)) {
        auto <- sprintf("forcats::fct_in%s", rlang::arg_match(auto))
        rhs_expr <- rlang::expr((!!rlang::parse_expr(auto))(!!rlang::sym(var)))
    } else {
        rhs_expr <- rlang::expr(factor(!!rlang::sym(var), !!new_lvls))
    }
    if (is.null(name)) {
        name <- sprintf("%s.reord", var)
    }
    vars_expr <- rlang::list2(!!name := rhs_expr)
    expr <- mutate_expr_i(expr, vars_expr, data, .after = var)
    eval_code(expr)
}


#' Convert missing values to categorical variables
#'
#' Turn \code{<NA>} in categorical variables into \code{"(Missing)"};
#' numeric variables will be converted to categorical variables where numeric
#' values as \code{"(Observed)"} and \code{NA} as \code{"(Missing)"}.
#'
#' @param data a dataframe with the columns to convert
#'        its missing values into categorical
#' @param vars  a character vector of the variables in \code{data}
#'        for conversion of missing values
#' @param names a character vector of names for the new variables
#' @return original dataframe containing new columns of the converted variables
#'        for the missing values with tidyverse code attached
#' @rdname missing_to_cat
#' @seealso \code{\link{code}}
#' @examples
#' missing <- missing_to_cat(iris, vars = c("Species", "Sepal.Length"))
#' cat(code(missing))
#' head(missing)
#'
#' @author Zhaoming Su
#' @export
missing_to_cat <- function(data, vars, names = NULL) {
    expr <- rlang::enexpr(data)
    if (is.null(names)) {
        names <- sprintf("%s.miss", vars)
    }
    vars_expr <- purrr::map(vars, function(x) {
        .data <- if (is_survey(data)) data$variables else data
        if (is.numeric(.data[[x]])) {
            rlang::expr(factor(dplyr::case_match(
                !!rlang::sym(x),
                NA ~ "(Missing)",
                .default = "(Observed)"
            )))
        } else {
            rlang::expr(forcats::fct_na_value_to_level(!!rlang::sym(x), "(Missing)"))
        }
    }) |> rlang::set_names(names)
    expr <- mutate_expr_i(expr, vars_expr, data, .after = vars)
    eval_code(expr)
}


#' Convert variables to date-time
#'
#' @param data a dataframe with the variables to convert
#' @param vars a character vector of column names to convert
#' @param ord a character vector of date-time formats
#' @param names a character vector of names for the created variables
#' @param tz a time zone name (default: local time zone). See
#'        \code{\link[base]{OlsonNames}}
#' @return original dataframe containing new columns of the
#'         converted variables with tidyverse code attached
#' @rdname convert_to_datetime
#' @seealso \code{\link{code}}
#' @export
#' @author Zhaoming Su
convert_to_datetime <- function(data, vars, ord = NULL, names = NULL, tz = "") {
    expr <- rlang::enexpr(data)
    if (is.null(names)) {
        names <- sprintf("%s.datetime", vars)
    }
    if (!requireNamespace("lubridate", quietly = TRUE)) {
        stop("Please install lubridate to use this function.")
    }
    vars_expr <- purrr::map(vars, function(x) {
        if (is.null(ord)) {
            rlang::expr(lubridate::as_datetime(!!rlang::sym(x), tz = !!tz))
        } else {
            rlang::expr(lubridate::parse_date_time(
                !!rlang::sym(x),
                orders = !!parse_datetime_order(ord),
                tz = !!tz
            ))
        }
    }) |> rlang::set_names(names)
    expr <- mutate_expr_i(expr, vars_expr, data, .after = vars)
    eval_code(expr)
}


#' Convert variables to dates
#'
#' @param data a dataframe with the variables to convert
#' @param vars a character vector of column names to convert
#' @param ord a character vector of date-time formats
#' @param names a character vector of names for the created variables
#' @return original dataframe containing new columns of the
#'         converted variables with tidyverse code attached
#' @rdname convert_to_date
#' @seealso \code{\link{code}}
#' @export
#' @author Zhaoming Su
convert_to_date <- function(data, vars, ord = NULL, names = NULL) {
    expr <- rlang::enexpr(data)
    if (is.null(names)) {
        names <- sprintf("%s.date", vars)
    }
    if (!requireNamespace("lubridate", quietly = TRUE)) {
        stop("Please install lubridate to use this function.")
    }
    vars_expr <- purrr::map(vars, function(x) {
        if (is.null(ord)) {
            rlang::expr(lubridate::as_date(!!rlang::sym(x)))
        } else {
            rlang::expr(lubridate::parse_date_time(
                !!rlang::sym(x),
                orders = !!parse_datetime_order(ord)
            ) |> as.Date())
        }
    }) |> rlang::set_names(names)
    expr <- mutate_expr_i(expr, vars_expr, data, .after = vars)
    eval_code(expr)
}
