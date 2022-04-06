#' Data dictionaries
#'
#' Read a data dictionary from file, attach to a dataset (plus utility functions).
#' These can then be used by other methods (such as plots) to automatically create
#' axes, etc.
#'
#' @section
#' Units and codes:
#'
#' For numeric variables, the dictionary can specify the units used in the measurements.
#'
#' For categorical variables, often these are coded rather than printed in full in the dataset. Data dictionaries may specify a column containing the codes (with a separator) and
#'
#'
#'
#' @param file path to a file
#' @param name name of the column containing the variable name
#' @param type column containing the variable type
#' @param title the column containing a short, human-readable title for the variable - if blank, the variable name will be used instead
#' @param description name of the column containing the variable description
#' @param units column containing units (for numeric variables only)
#' @param codes column containing factor codes
#' @param values column containing factor values - these should be in the same order
#' @param level_separator the separator used to separate levels in `codes` and `values` columns, default "|". Can optionally be a vector of length 2, in which case the first is used for `codes` and the second for `values`.
#' @param ... additional arguments, passed to `smart_read`
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

    if (!missing(title) && title != "title")
        dict <- dplyr::rename(dict, title = !!title)

    if (!missing(description) && description != "description")
        dict <- dplyr::rename(dict, decription = !!description)

    # TODO: all the other columns too

    dict_list <- lapply(seq_along(dict$name),
        function(x) dict_row(dict[x, ], sep = level_separator))
    names(dict_list) <- dict$name
    dict_list
}

dict_row <- function(x, sep) {
    sep <- rep(sep, length = 2L)
    cn <- colnames(x)
    row <- list(
        name = as.character(x$name),
        type = if ("type" %in% cn) as.character(x$type) else {
            if ("units" %in% cn && !is.na(x$units)) "numeric"
            else if (all(c("codes", "values") %in% cn) && !is.na(x$codes) && !is.na(x$values)) "factor"
            else NULL
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
    class(row) <- "dict_var"
    row
}

#' @export
print.dict_var <- function(x, ...) {
    cat(
        sprintf("%s: %s %s\n", x$name, x$title,
            ifelse(is.null(x$type), "",
                sprintf("[%s]", x$type)
            )
        )
    )
    if (!is.null(x$description)) cat(x$description, "\n")
    if (!is.null(x$units)) cat(sprintf("Units: %s\n", x$units))
}

#' Apply a data dictionary to dataset
#'
#' @param data a dataset (dataframe, tibble)
#' @param dict a dictionary (created using `read_dictionary()`)
#' @md
#' @export
#' @rdname dictionary
apply_dictionary <- function(data, dict) {

    for (d in dict) {
        if (!d$name %in% names(data)) next
        data[[d$name]] <- add_var_attributes(data[[d$name]], d)
    }

    data
}

add_var_attributes <- function(x, d)
    UseMethod("add_var_attributes")

#' @export
add_var_attributes.default <- function(x, d) x

#' @export
add_var_attributes.numeric <- function(x, d) {
    if (!is.null(d$type) && d$type %in% c("factor", "categorical")) {
        x <- factor(x)
        return(add_var_attributes(x, d))
    }

    # add units
    if (!requireNamespace("units", quietly = TRUE)) return(x)
    # xunits <- try(units::ud_units[[d$units]], silent = TRUE)
    # if (inherits(xunits, "try-error")) {
    #     print(xunits)
    #     warning("Unable to create units", d$uinits)
    #     return(x)
    # }
    try(units(x) <- d$units, silent = TRUE)
    x
}

#' @export
add_var_attributes.factor <- function(x, d) {
    lbls <- lapply(d$coding$code, function(x) x)
    names(lbls) <- d$coding$value
    do.call(forcats::fct_recode, c(list(x), lbls))
}

#' Check data has dictionary attached
#' @rdname dictionary
#' @export
has_dictionary <- function(data)
    !is.null(attr(data, "dictionary")) &&
    is.data.frame(attr(data, "dictionary"))

#' Get data dictionary from data
#' @rdname dictionary
#' @export
get_dictionary <- function(data) attr(data, "dictionary", exact = TRUE)
