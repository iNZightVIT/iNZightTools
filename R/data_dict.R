#' Read a file as a data dictionary
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
