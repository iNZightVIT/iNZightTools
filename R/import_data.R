##' A simple function that magically imports a file, irrespective of type.
##'
##' @title iNZight Smart Read
##' @param file the file path to read
##' @param ext file extension, namely "csv" or "txt"
##' @param preview logical, if \code{TRUE} only the first few rows of
##'   the data will be returned
##' @param column_types vector of column types (see \code{?readr::read_csv})
##' @param ... additional parameters passed to read_* functions
##' @return a dataframe with attributes
##' @author Tom Elliott
##' @export
smart_read <- function(file, ext = tools::file_ext(file), preview = FALSE,
                       column_types = NULL, ...) {
    type <- guess_type(ext)
    fun <- eval(parse(text = sprintf("read_%s", type)))
    d <- fun(file, ext = ext, preview = preview,
        column_types = column_types, ...
    )

    ## now the data is read, convert things to factors etc
    d <- strings_to_factors(d)

    ## ensure any numeric->categorical changes retain numeric order of levels
    d <- validate_type_changes(d, column_types)

    if (preview)
      class(d) <- c('inz.preview', class(d))
    if (is.null(attr(d, "name")))
      attr(d, "name") <- tools::file_path_sans_ext(basename(file))
    d
}

guess_type <- function(ext) {
    switch(ext,
        "xls" = "excel",
        "xlsx" = "excel",
        "sav" = "spss",
        "dta" = "stata",
        "sas7bdat" = "sas",
        "xpt" = "sas",
        "txt" = "meta",
        "csv" = "meta", ## -> metadata_read.R
        "unknown"
    )
}

read_unknown <- function(file, ...) {
  warning("Unable to read file: ", file)
  return(NULL)
}

read_dlm <- function(file, ext = tools::file_ext(file), preview = FALSE,
                     column_types = NULL, encoding, delimiter, decimal_mark,
                     grouping_mark, convert.to.factor = TRUE,
                     ...) {

    named.args <- list(...)

    if (is.null(named.args$comment))
        named.args$comment <- "#"

    if (preview)
        named.args <- c(list(n_max = 10), named.args)

    if (missing(delimiter))
        delimiter <- ifelse(ext == "csv", ",", " ")

    if (ext != "csv" || delimiter != ",")
        named.args <- c(list(delim = delimiter), named.args)
    else if (ext == "txt")
        named.args <- c(list(delim = " "), named.args)

    locale <- list()
    if (!missing(encoding))
        locale$encoding <- escape_string(encoding)

    if (!missing(decimal_mark))
        locale$decimal_mark <- escape_string(decimal_mark)

    if (!missing(grouping_mark))
        locale$grouping_mark <- escape_string(grouping_mark)

    ## quote character arguments (x = z -> x = "z")
    named.args <- lapply(named.args,
        function(x) {
            if (is.character(x)) escape_string(x)
                else x
        }
    )

    ctypes <- parse_coltypes(column_types)
    if (ctypes != "NULL")
        named.args <- c(named.args, list(col_types = "COLTYPES"))

    if (length(locale) > 0)
        named.args$locale <- sprintf("readr::locale(%s)",
            paste(names(locale), locale,
                sep = " = ",
                collapse = ", "
            )
        )

    if (length(named.args) > 0)
        args <- paste(
            "file,",
            paste(names(named.args), named.args,
                collapse = ", ",
                sep = " = "
            )
        )
    else
        args <- "file"

    exp <- ~FUN(ARGS)
    exp <- replaceVars(exp,
        FUN = sprintf("readr::read_%s",
            ifelse(ext == "csv" && delimiter == ",", "csv", "delim")
        ),
        ARGS = args,
        COLTYPES = ctypes
    )

    interpolate(exp, file = file)
}

read_excel <- function(file, ext, preview = FALSE, column_types, ...) {
    named.args <- list(...)

    if (!missing(column_types))
        named.args <- c(list(col_types = column_types), named.args)

    if (preview)
        named.args <- c(list(n_max = 10), named.args)

    if (length(named.args) > 0)
        args <- paste("file,",
            paste(names(named.args), named.args,
                collapse = ", ",
                sep = " = "
            )
        )
    else
        args <- "file"

    exp <- ~readxl::read_excel(ARGS)
    exp <- replaceVars(exp, ARGS = args)

    interpolate(exp, file = file)
}

read_spss <- function(file, ext, preview = FALSE, column_types) {
    exp <- ~haven::read_sav(file)

    interpolate(exp, file = file)
}

read_stata <- function(file, ext, preview = FALSE, column_types) {
    exp <- ~haven::read_dta(file)
    interpolate(exp, file = file)
}

read_sas <- function(file, ext, preview = FALSE, column_types) {
    exp <- ~FUN(file)
    exp <- replaceVars(exp,
        FUN = switch(ext,
            "xpt" = "haven::read_xpt",
            "haven::read_sas"
        )
    )
    interpolate(exp, file = file)
}

escape_string <- function(x) sprintf("\"%s\"", x)

quote_varname <- function(x, q = "`") {
    ## contains any non alphanumeric characters, OR first character is number
    xs <- grepl("[^a-zA-Z0-9]", x) | grepl("^[0-9]", x)
    if (any(xs)) {
        x[xs] <- paste0(q, x[xs], q)
    }
    x
}

#' Checks if the complete file was read or not.
#'
#' @title Is Preview
#' @param df data to check
#' @return logical
#' @export
is_preview <- function(df) inherits(df, "inz.preview")

## Convert string/character columns of a dataframe to factors,
## adding the necessary code along the way.
strings_to_factors <- function(x, ctypes) {
    chars <- sapply(x, is.character)
    if (!any(chars)) return(x)

    ## mutate(name = factor(name))
    TEMP_RESULT <- x
    rm(x) # clean up
    charnames <- names(TEMP_RESULT)[chars]

    expr <- paste(
        "TEMP_RESULT %>% dplyr::mutate(",
        paste("\"", charnames, "\" = as.factor(",
            quote_varname(charnames),
            ")",
            sep = "",
            collapse = ", "
        ),
        ")",
        sep = ""
    )

    res <- eval(parse(text = expr))
    # prepend original code
    if (!is.null(code(TEMP_RESULT)))
        attr(res, "code") <-
            gsub("TEMP_RESULT", 
                paste(code(TEMP_RESULT), collapse="\n"), 
                expr
            )
    res
}

validate_type_changes <- function(x, column_types) {
    ctypes <- parse_coltypes(column_types)
    if (ctypes == "NULL") return(x)

    ## ensure that numeric -> categorical order is in numerical order
    # if (!any(column_types == "c")) return(x)

    TEMP_RESULT <- x
    
    conv <- sapply(names(column_types), function(name) {
        type <- column_types[[name]]
        col <- TEMP_RESULT[[name]]
        switch(type,
            "n" = {
                ## Convert factor to numeric
                if (is.numeric(col)) return("")
                sprintf("%s = as.numeric(%s)", name, name)
            },
            "c" = {
                ## Convert numeric to factor
                ncol <- suppressWarnings(as.numeric(as.character(col)))
                if (any(is.na(ncol)) || !all(ncol == col))
                    return("")
                lvls <- sort(unique(ncol))
                if (is.factor(col)) {
                    # relevel
                    if (!all(levels(col) == lvls)) {
                        sprintf("%s = forcats::fct_relevel(%s, c('%s'))",
                            name, name,
                            paste(lvls, collapse = "', '")
                        )
                    }
                } else {
                    sprintf("%s = factor(%s, levels = c('%s'))",
                        name, name, 
                        paste(lvls, collapse = "', '")
                    )
                }
            }
        )
    })

    if (all(conv == "")) return(x)
    conv <- conv[conv != ""]

    expr <- sprintf("TEMP_RESULT %s dplyr::mutate(%s)",
        "%>%",
        paste(conv, sep = ",")
    )

    res <- eval(parse(text = expr))
    attr(res, "code") <-
        gsub("TEMP_RESULT", paste(code(TEMP_RESULT), collapse="\n"), expr)
    res
}


parse_coltypes <- function(column_types = NULL) {
    if (is.null(column_types)) return("NULL")
    
    if (!is.null(names(column_types))) {
        ctypes <- paste(
            "readr::cols(",
            paste(names(column_types), " = '", column_types, "'",
                sep = "",
                collapse = ", "
            ),
            ")",
            sep = ""
        )
    } else {
        ctypes <- paste("readr::cols('", column_types, "')",
            sep = "",
            collapse = ""
        )
    }

    ctypes
}
