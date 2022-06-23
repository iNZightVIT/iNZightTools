#' Import linked data into an `inzdf` object
#'
#' @param x a linked specification file or vector of data set paths
#' @param schema a list describing the schema/relationships between the files
#' @param con a database connection to load the linked data into
#' @param name the name of the data set collection
#' @param ... additional arguments passed to data reading function `smart_read()`
#'
#' @return an `inzdf` object
#' @md
#' @export
load_linked <- function(x, schema, con, name = deparse(substitute(con)), ...) {
    if (!inherits(x, "inzlnk_spec")) {
        if (length(x) == 1L && tools::file_ext(x) == "inzlnk") {
            x <- read_link_spec(x)
        } else {
            x <- link_spec(x, schema, name = name)
        }
    }

    if (is.null(x$schema)) stop("Need to specify a schema")
    if (missing(con)) stop("Please specify a database connection")

    if (!is.null(x$dictionary)) {
        message("Loading dictionary ...")
        dict <- do.call(read_dictionary, x$dictionary)
        x$dictionary <- dict
        message("Dictionary loaded!")
    }

    var_attrs <- lapply(names(x$files),
        function(f) {
            d <- smart_read(x$files[f], ...)
            if (!is.null(x$dictionary))
                d <- apply_dictionary(d, x$dictionary)
            vf <- lapply(d, function(x)
                utils::modifyList(list(class = class(x)), as.list(attributes(x)))
            )
            on.exit(rm(d))
            DBI::dbWriteTable(con, f, d)
            vf
        }
    )
    names(var_attrs) <- names(x$files)

    inzdf(con, name = name, schema = x$schema, var_attrs, dictionary = x$dictionary)
}

table_spec <- function(x) {
    x$links_to <- lapply(x$links_to, unlist)
    x
}

read_link_spec <- function(x, name = deparse(substitute(x))) {
    z <- yaml::read_yaml(x)
    files <- unlist(z$files)
    schema <- stats::setNames(lapply(z$schema, table_spec), names(z$schema))
    dictionary <- z$dictionary

    link_spec(files, schema, dictionary, name)
}

link_spec <- function(files, schema, dictionary = NULL, name) {
    structure(
        list(
            files = files,
            schema = schema,
            dictionary = dictionary,
            name = name
        ),
        class = "inzlnk_spec"
    )
}
