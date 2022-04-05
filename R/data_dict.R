#' Data dictionaries
#'
#' Read a data dictionary from file, attach to a dataset (plus utility functions).
#' These can then be used by other methods (such as plots) to automatically create
#' axes, etc.
#'
#' @section
#' Columns:
#' The following columns are understood:
#'
#' * units - these will be used to label axes / legends
#'
#' @param file path to a file
#' @param id name of the column containing the ID, if missing will be guessed
#' @param name name of the column containing the human-readable variable name
#' @param description name of the column containing the variable description
#' @md
#' @rdname dictionary
#' @export
read_dictionary <- function(file, id, name, description, ...) {
    dict <- smart_read(file, ...)

    # now to parse some things ...
    if (!missing(id) && id != "id") {
        dict <- dplyr::rename(dict, id = !!id)
    } else if (!"id" %in% names(dict)) {
        ## TODO: guess_id()
        stop("Please specify `id`")
    }

    if (!missing(name) && name != "name")
        dict <- dplyr::rename(dict, name = !!name)

    if (!missing(description) && description != "description")
        dict <- dplyr::rename(dict, decription = !!description)

    dict
}

#' Add data dictionary to dataset
#'
#' @param data a dataset (dataframe, tibble)
#' @param dict a dictionary (created using `read_dictionary()`)
#' @md
#' @export
#' @rdname dictionary
add_dictionary <- function(data, dict) {
    attr(data, "dictionary") <- dict
    data
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
