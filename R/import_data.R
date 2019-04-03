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
smart_read <- function(file, ext = tools::file_ext(file), preview = FALSE, column_types, ...) {
    type <- guess_type(ext)
    fun <- eval(parse(text = sprintf("read_%s", type)))
    d <- fun(file, ext = ext, preview = preview, column_types = column_types, ...)
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
           "unknown")
}

read_unknown <- function(file, ...) {
  warning("Unable to read file: ", file)
  return(NULL)
}

read_dlm <- function(file, ext = tools::file_ext(file), preview = FALSE, column_types,
                     encoding, delimiter, decimal_mark, grouping_mark,
                     convert.to.factor = TRUE,
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
                         })

    ctypes <- ""
    if (!missing(column_types)) {
        named.args <- c(list(col_types = "COLTYPES"))

        if (!is.null(names(column_types))) {
            ctypes <- paste("readr::cols(", names(column_types), " = '", column_types, "')",
                            sep = "", collapse = ", ")
        } else {
            ctypes <- paste("readr::cols('", column_types, "')", sep = "", collapse = "")
        }
    }

    if (length(locale) > 0)
        named.args$locale <- sprintf("readr::locale(%s)",
                                     paste(names(locale), locale,
                                           sep = " = ", collapse = ", "))

    if (length(named.args) > 0)
        args <- paste("file,",
                      paste(names(named.args), named.args,
                            collapse = ", ", sep = " = "))
    else
        args <- "file"

    exp <- ~FUN(ARGS)
    exp <- replaceVars(exp,
                       FUN = sprintf("readr::read_%s",
                                     ifelse(ext == "csv" && delimiter == ",",
                                            "csv", "delim")),
                       ARGS = args,
                       COLTYPES = ctypes)

    TEMP_RESULT <- interpolate(exp, file = file)
    if (!convert.to.factor) return(TEMP_RESULT)

    chars <- sapply(TEMP_RESULT, is.character)
    if (!any(chars)) return(TEMP_RESULT)

    ## mutate(name = factor(name))
    charnames <- names(TEMP_RESULT)[chars]
    expr2 <- paste(
        "TEMP_RESULT %>% dplyr::mutate(",
        paste("\"", charnames, "\" = as.factor(",
              quote_varname(charnames),
              ")",
              sep = "", collapse = ", "),
        ")", sep = "")

    res2 <- eval(parse(text = expr2))
    attr(res2, "code") <-
        gsub("TEMP_RESULT", paste(code(TEMP_RESULT), collapse="\n"), expr2)
    res2
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
                            collapse = ", ", sep = " = "))
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
    exp <- ~haven::read_EXT(file)
    exp <- replaceVars(exp,
        EXT = switch(ext,
            "xpt" = "xpt",
            "sas"
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

