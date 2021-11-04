#' A simple function that magically imports a file, irrespective of type.
#'
#' The smart read function understands the following:
#' * delimited (.csv, .txt)
#' * excel files (.xls, .xlsx)
#' * spss files (.sav)
#' * stata files (.dta)
#' * SAS files (.sas7bdat, .xpt)
#' * R data files (.rds)
#' * JSON files (.json)
#'
#' @title iNZight Smart Read
#' @param file the file path to read
#' @param ext file extension, namely "csv" or "txt"
#' @param preview logical, if \code{TRUE} only the first few rows of
#'   the data will be returned
#' @param column_types vector of column types (see \code{?readr::read_csv})
#' @param ... additional parameters passed to read_* functions
#' @return a dataframe with attributes
#' @author Tom Elliott
#' @md
#' @export
smart_read <- function(file, ext = tools::file_ext(file), preview = FALSE,
                       column_types = NULL, ...) {

    if (grepl("^https?://", file)) file <- url_to_temp(file)

    type <- guess_type(ext)
    fun <- eval(parse(text = sprintf("read_%s", type)))
    d <- fun(file, ext = ext, preview = preview,
        column_types = column_types, ...
    )
    # if the first 1000+ rows are missing (NA), they are by default
    # read as logical - here we re-read as character:
    if (!is.null(attr(d, "bad_guess"))) {
        column_types <- c(
            column_types,
            attr(d, "bad_guess")
        )
        d <- fun(file, ext = ext, preview = preview,
            column_types = column_types, ...
        )
    }
    attrs <- attributes(d)
    attr_to_keep <- c("available.sheets")
    if (any(names(attrs) %in% attr_to_keep))
        attrs <- attrs[names(attrs) %in% attr_to_keep]
    else
        attrs <- NULL

    ## now the data is read, convert things to factors etc
    d <- convert_strings(d)

    ## ensure any numeric->categorical changes retain numeric order of levels
    d <- validate_type_changes(d, column_types)

    ## ensure variable names are valid
    d <- validate_names(d)

    if (preview)
      class(d) <- c("inz.preview", class(d))
    if (is.null(attr(d, "name")))
      attr(d, "name") <- tools::file_path_sans_ext(basename(file))

    # replace any attributes
    if (!is.null(attrs)) {
        attributes(d) <- c(attributes(d), attrs)
    }

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
        "rds" = "rds",
        "json" = "json",
        "txt" = "meta",
        "csv" = "meta", ## -> metadata_read.R
        "unknown"
    )
}

read_unknown <- function(file, ...) {
    warning("Unable to read file: ", file)
    return(NULL)
}

read_dlm <- function(file,
                     ext = tools::file_ext(file),
                     preview = FALSE,
                     column_types = NULL,
                     encoding,
                     delimiter,
                     decimal_mark,
                     grouping_mark,
                     convert.to.factor = TRUE,
                     ...) {

    named.args <- list(...)

    if (is.null(named.args$comment))
        named.args$comment <- "#"

    if (preview)
        named.args <- c(list(n_max = 100), named.args)

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

    if (is.null(named.args$col_types))
        named.args <- c(
            named.args,
            list(col_types = "readr::cols()")
        )

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

    if (utils::packageVersion("readr") >= numeric_version('2.0.0'))
        args <- paste0(args, ", lazy = FALSE")

    exp <- ~FUN(ARGS)
    exp <- replaceVars(exp,
        FUN = sprintf("readr::read_%s",
            ifelse(ext == "csv" && delimiter == ",", "csv", "delim")
        ),
        ARGS = args,
        COLTYPES = ctypes
    )

    x <- suppressWarnings(
        interpolate(exp, file = file)
    )
    if (!is.null(attr(x, "problems"))) {
        # logical -> character
        spec <- attr(x, "spec")$cols
        is_bad <- sapply(spec, function(x) methods::is(x, "collector_logical"))
        if (any(is_bad)) {
            ct <- structure(
                as.list(rep("c", sum(is_bad))),
                .Names = names(spec)[is_bad]
            )
            attr(x, "bad_guess") <- ct
        }
    }
    x
}

#' @import readxl
read_excel <- function(file,
                       ext,
                       preview = FALSE,
                       column_types,
                       sheet = NULL,
                       ...) {
    named.args <- list(...)

    if (!missing(column_types) && !is.null(column_types))
        named.args <- c(list(col_types = column_types), named.args)

    if (preview)
        named.args <- c(list(n_max = 10), named.args)

    if (!is.null(sheet))
        named.args <- c(list(sheet = "sheetname"), named.args)

    if (!is.null(named.args$na))
        named.args$na <- escape_string(named.args$na)

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

    res <- interpolate(exp, file = file, sheetname = sheet)
    if (preview)
        attr(res, "available.sheets") <- readxl::excel_sheets(file)
    res
}

#' List of available sheets from a file
#'
#' @param x a dataframe from \code{smart_read}
#' @return vector of sheet names, or NULL
#' @author Tom Elliott
#' @export
sheets <- function(x) {
    attr(x, "available.sheets")
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

read_rds <- function(file, ext, preview = FALSE, column_types) {
    exp <- ~readRDS(file)
    interpolate(exp, file = file)
}

read_json <- function(file, ext, preview = FALSE, column_types) {
    if (!requireNamespace("jsonlite", quietly = TRUE))
        stop("Please install the `jsonlite` package to read JSON files.")

    exp <- ~jsonlite::fromJSON(f)
    tryCatch(x <- interpolate(exp, f = file),
        error = function(e) {
            stop("Unable to read file:\n", e)
        }
    )

    x
}

escape_string <- function(x) sprintf("\"%s\"", x)

quote_varname <- function(x, q = "`") {
    ## contains any non alphanumeric characters,
    ## OR first character is number
    xs <- grepl("[^a-zA-Z0-9_]", x) | grepl("^[0-9_]", x)
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
convert_strings <- function(x, ctypes) {
    chars <- sapply(x, is.character)
    if (!any(chars)) return(x)

    ## mutate(name = factor(name))
    TEMP_RESULT <- x
    rm(x) # clean up
    charnames <- names(TEMP_RESULT)[chars]

    types <- sapply(TEMP_RESULT[charnames], readr::guess_parser)
    convert_fn <- sapply(types,
        function(type)
            switch(type,
                "date" = "as.Date",
                "time" = "hms::as_hms",
                "datetime" = "as.POSIXct",
                "double" = "as.numeric",
                "character" = ,
                "as.factor"
            )
    )

    convert_fun <- as.factor(convert_fn)
    convert_list <- tapply(charnames, convert_fun, c)
    convert_exprs <- lapply(names(convert_list),
        function(fn) {
            sprintf("dplyr::mutate_at(%s, %s)",
                paste(capture.output(dput(convert_list[[fn]])), collapse = " "),
                fn)
        }
    )

    expr <- paste(
        "TEMP_RESULT %>%",
        paste(convert_exprs, collapse = " %>% ")
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
                # skip if ONE missing (not both)
                colm <- ncol == col | (is.na(col) + is.na(ncol)) == 2L
                colm <- ifelse(is.na(colm), FALSE, colm)
                if (!all(colm))
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
        paste(conv, collapse = ", ")
    )

    res <- eval(parse(text = expr))
    attr(res, "code") <-
        gsub("TEMP_RESULT",
            paste(code(TEMP_RESULT), collapse="\n"),
            expr
        )
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

validate_names <- function(x) {
    # check if any columns need renaming:
    names <- names(x)
    new <- make.names(gsub("\\s+", "_", names))
    if (all(new == names)) return(x)

    # if the last character is a dot (but only in `new`), remove it
    remove_dot <- grepl("[.]$", new) & !grepl("[.]", names)
    if (any(remove_dot))
        new[remove_dot] <- gsub("[.]$", "", new[remove_dot])

    # now ensure names are all UNIQUE
    new <- make.names(new, unique = TRUE)

    TEMP_RESULT <- x
    w <- which(new != names)
    conv <- paste0(new[w], " = \"", names[w], "\"")
    expr <- sprintf(
        "TEMP_RESULT %s dplyr::rename(%s)",
        "%>%",
        paste(conv, collapse = ", ")
    )

    res <- eval(parse(text = expr))
    attr(res, "code") <-
        gsub("TEMP_RESULT",
            paste(code(TEMP_RESULT), collapse="\n"),
            expr
        )
    res
}

#' Load object(s) from an Rdata file
#'
#' @param file path to an rdata file
#'
#' @return list of data frames, plus code
#' @seealso \code{\link{save_rda}}
#' @author Tom Elliott
#' @export
load_rda <- function(file) {
    e <- new.env()
    load(file, envir = e)
    keep <- sapply(names(e), function(n) is.data.frame(e[[n]]))
    res <- lapply(names(e)[keep], function(n) e[[n]])
    names(res) <- names(e)[keep]
    attr(res, "code") <- sprintf("load('%s')", file)
    res
}

#' Save an object with, optionally, a (valid) name
#'
#' @param data the data frame to save
#' @param file where to save it
#' @param name optional, the name the data will have in the rda file
#'
#' @return logical, should be TRUE, along with code for the save
#' @seealso \code{\link{load_rda}}
#' @author Tom Elliott
#' @export
save_rda <- function(data, file, name) {
    if (missing(file)) stop("Please specify a file location")
    data_name <- deparse(substitute(data))
    if (!missing(name))
        name <- create_varname(name)
    else
        name <- data_name

    e <- new.env()
    e[[name]] <- data

    exp <- sprintf("save(%s, file = '%s')", name, file)
    eval(parse(text = exp), envir = e)
    structure(TRUE, code = exp)
}

#' Download URL to temp file
#'
#' @param url where the file lives on the internet
#' @return the location of a (temporary) file location
#' @author Tom Elliott
url_to_temp <- function(url) {
    name <- basename(url)
    name <- gsub("%20", "_", name)
    name <- create_varname(name)
    dir <- tempdir()
    file <- file.path(dir, name)
    utils::download.file(url, file, quiet = TRUE)
    file
}
