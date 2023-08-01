#' Data Dictionaries
#'
#' This function reads a data dictionary from a file and attaches it to a
#' dataset. The attached data dictionary provides utility functions that can be
#' used by other methods, such as plots, to automatically create axes and more.
#'
#' @param file The path to the file containing the data dictionary.
#' @param name The name of the column containing the variable name.
#' @param type The name of the column containing the variable type.
#' @param title The name of the column containing a short, human-readable title
#'        for the variable. If blank, the variable name will be used instead.
#' @param description The name of the column containing the variable
#'        description.
#' @param units The name of the column containing units (for numeric
#'        variables only).
#' @param codes The name of the column containing factor codes (for categorical
#'        variables only).
#' @param values The name of the column containing factor values
#'        corresponding to the codes. These should be in the same order
#'        as the codes.
#' @param level_separator The separator used to separate levels in `codes` and
#'        `values` columns. The default separator is "|".
#'        Alternatively, you can provide a vector of length 2, where the first
#'        element is used for `codes` and the second element for `values`.
#' @param ... Additional arguments, passed to `smart_read`.
#' @return The dataset with the attached data dictionary.
#' @md
#' @rdname dictionary
#' @export
read_dictionary <- function(file,
                            name = "name",
                            type = "type",
                            title = "title",
                            description = "description",
                            units = "units",
                            codes = "codes",
                            values = "values",
                            level_separator = "|",
                            ...) {
    dict <- smart_read(file, ...)

    # convert all factors to strings
    dict <- do.call(
        data.frame,
        c(
            lapply(dict, function(x) if (is.factor(x)) as.character(x) else x),
            list(stringsAsFactors = FALSE)
        )
    )

    ## TODO: explore use of packages:
    # * 'units'
    # * 'expss'

    # now to parse some things ...
    if (name != "name") {
        dict <- dplyr::rename(dict, name = !!name)
    } else if (!"name" %in% names(dict)) {
        ## TODO: guess_id()
        stop("Please specify `name`")
    }

    if (!missing(type) && type != "type") {
        dict <- dplyr::rename(dict, type = !!type)
    }

    if (!missing(title) && title != "title") {
        dict <- dplyr::rename(dict, title = !!title)
    }

    if (!missing(description) && description != "description") {
        dict <- dplyr::rename(dict, description = !!description)
    }

    if (!missing(codes) && codes != "codes") {
        dict <- dplyr::rename(dict, codes = !!codes)
    }

    if (!missing(values) && values != "values") {
        dict <- dplyr::rename(dict, values = !!values)
    }
    #'
    # TODO: all the other columns too

    dict_list <- lapply(
        seq_along(dict$name),
        function(x) dict_row(dict[x, ], sep = level_separator)
    )
    names(dict_list) <- tolower(dict$name)
    structure(dict_list, class = "dictionary")
}

#' Print a dictionary object
#'
#' @param x A `dictionary` object.
#' @param kable If `TRUE`, the output will be formatted using kable.
#' @param include_other  If `TRUE`, additional variables will be included in
#'        the output.
#' @md
#' @export
#' @rdname dictionary
print.dictionary <- function(x, kable = FALSE, include_other = TRUE, ...) {
    dots <- list(...)
    if (kable && requireNamespace("knitr", quietly = TRUE)) {
        if (is.null(dots$code_sep)) {
            knitr::kable(as_tibble(x, include_other = include_other, code_sep = "<br/>", ...), ...)
        } else {
            knitr::kable(as_tibble(x, include_other = include_other, ...), ...)
        }
    } else {
        print(as_tibble(x, include_other = include_other, ...), ...)
    }
}

#' as_tibble
#'
#' @name as_tibble
#' @rdname as_tibble
#' @keywords internal
#' @export
#' @importFrom tibble as_tibble
NULL


#' Convert dictionary object to a 'tibble'
#' @param x A `dictionary` object.
#' @param n Numeric, the number of rows to convert.
#' @param include_other If `TRUE`, other variables with be included in the
#'        tibble.
#' @param code_sep The separator used between codes and values.
#' @rdname as_tibble
#' @export
as_tibble.dictionary <- function(x, n = length(x),
                                 include_other = TRUE,
                                 code_sep = ifelse(interactive(), "|", "\n"),
                                 ...) {
    x <- lapply(x[1:min(n, length(x))], function(y) {
        if (!is.null(y$coding)) {
            coding <- lapply(y$coding, paste, collapse = code_sep)
            y$coding <- NULL
            y$code <- coding$code
            y$value <- coding$value
        }
        if (include_other && !is.null(y$other)) {
            other <- y$other
            y$other <- NULL
            y <- c(y, other)
        } else {
            y$other <- NULL
        }
        y
    })
    vars <- unique(do.call(c, lapply(x, names)))
    x <- lapply(x, function(z) {
        lapply(
            stats::setNames(z[vars], vars),
            function(y) if (is.null(y)) NA else (y)
        )
    })
    tibble::as_tibble(lapply(purrr::transpose(x), unlist))
}

#' @rdname dictionary
#' @param i Subset index.
#' @export
`[.dictionary` <- function(x, i, ...) {
    structure(unclass(x)[i], class = class(x))
}

dict_row <- function(x, sep) {
    sep <- rep(sep, length = 2L)
    cn <- colnames(x)
    row <- list(
        name = tolower(as.character(x$name)),
        type = if ("type" %in% cn) {
            switch(as.character(x$type),
                "number" = ,
                "numeric" = ,
                "float" = "numeric",
                "double" = "numeric",
                "integer" = "integer",
                "categorical" = ,
                "factor" = "factor",
                as.character(x$type)
            )
        } else {
            if ("units" %in% cn && !is.na(x$units)) {
                "numeric"
            } else if (all(c("codes", "values") %in% cn) && !is.na(x$codes) && !is.na(x$values)) {
                "factor"
            } else {
                NULL
            }
        },
        title = if ("title" %in% cn) as.character(x$title) else as.character(x$name),
        description = if ("description" %in% cn) as.character(x$description) else NULL
    )
    if ("units" %in% cn && !is.na(x$units)) {
        row$units <- as.character(x$units)
    } else if (all(c("codes", "values") %in% cn) && !is.na(x$codes) && !is.na(x$values)) {
        row$coding <- data.frame(
            code = trimws(strsplit(as.character(x$codes), sep[1], fixed = TRUE)[[1]]),
            value = trimws(strsplit(as.character(x$values), sep[2], fixed = TRUE)[[1]])
        )
    }
    cols <- c("name", "type", "title", "description", "units", "codes", "values")
    if (any(!cn %in% cols)) {
        row$other <- as.list(x[!cn %in% cols])
    }
    class(row) <- "dict_var"
    row
}

#' @export
print.dict_var <- function(x, ...) {
    cat(
        sprintf(
            "{%s}: %s %s\n", x$name, x$title,
            ifelse(is.null(x$type), "",
                sprintf("[%s]", x$type)
            )
        )
    )
    if (!is.null(x$description)) cat(x$description, "\n")
    if (!is.null(x$units)) cat(sprintf("Units: %s\n", x$units))
    if (!is.null(x$coding)) {
        cat("Factor codes:\n")
        print(x$coding, row.names = FALSE)
    }
}

#' Apply a data dictionary to dataset
#'
#' @param data A dataset (dataframe, tibble).
#' @param dict A dictionary (created using `read_dictionary()`).
#' @md
#' @export
#' @rdname dictionary
apply_dictionary <- function(data, dict) {
    if (!requireNamespace("expss", quietly = TRUE)) {
        stop("Please install suggested packages: 'expss'") # nocov
    }

    dnames <- names(data)
    names(data) <- tolower(names(data))

    for (d in dict) {
        if (!d$name %in% names(data)) next
        data[[d$name]] <- add_var_attributes(data[[d$name]], d)
    }

    # apply labels
    lbls <- lapply(dict, function(x) x$title)
    names(lbls) <- tolower(names(dict))
    lbls <- lbls[names(lbls) %in% names(data)]

    data <- do.call(expss::apply_labels, c(list(data), lbls))

    data
}

add_var_attributes <- function(x, d) {
    UseMethod("add_var_attributes")
}

#' @export
add_var_attributes.default <- function(x, d) x

#' @export
add_var_attributes.numeric <- function(x, d) {
    if (!is.null(d$type) && d$type %in% c("factor", "categorical")) {
        x <- factor(x)
        return(add_var_attributes(x, d))
    }

    # TODO: check if this all works with 'units' in Suggests
    # add units
    # if (!requireNamespace("units", quietly = TRUE)) {
    #     return(x)
    # }
    # xunits <- try(units::ud_units[[d$units]], silent = TRUE)
    # if (inherits(xunits, "try-error")) {
    #     print(xunits)
    #     warning("Unable to create units", d$uinits)
    #     return(x)
    # }
    try(units(x) <- units::as_units(d$units), silent = TRUE)
    x
}

#' @export
add_var_attributes.factor <- function(x, d) {
    lbls <- lapply(d$coding$code, function(x) x)
    names(lbls) <- d$coding$value

    if (!all(levels(x) %in% d$coding$code)) {
        # message("----- Mismatching levels")
        # print(d)
        # print(d$coding)
        # print(levels(x))

        levels(x) <- unique(c(d$coding$code, levels(x)))
    } else {
        levels(x) <- d$coding$code
    }

    res <- x
    tryCatch(res <- do.call(forcats::fct_recode, c(list(x), lbls)),
        warning = function(e) {
            message("------------------", e)
            print(d)
            print(d$coding)
            print(levels(x))
        }
    )
    res
}

#' Check data has dictionary attached
#' @rdname dictionary
#' @export
has_dictionary <- function(data) {
    !is.null(attr(data, "dictionary")) &&
        is.data.frame(attr(data, "dictionary"))
}

#' Get data dictionary from data
#' @rdname dictionary
#' @export
get_dictionary <- function(data) attr(data, "dictionary", exact = TRUE)


# a new data type for numeric variables *with codes*
# e.g., -1 = not applicable; 100 = 100+
