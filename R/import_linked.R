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

    lapply(names(x$files),
        function(f) {
            d <- smart_read(x$files[f], ...)
            on.exit(rm(d))
            DBI::dbWriteTable(con, f, d)
        }
    )

    inzdf(con, name = name, schema = x$schema)
}

table_spec <- function(x) {
    x$links_to <- lapply(x$links_to, unlist)
    x
}

read_link_spec <- function(x) {
    x <- yaml::read_yaml(x)
    spec <- list(
        files = x$files,
        schema = setNames(lapply(x$schema, table_spec), names(x$schema))
    )

    link_spec(x$files, x$schema, deparse(substitute(x)))
}

link_spec <- function(files, schema, name) {
    structure(
        list(
            files = files,
            schema = schema,
            name = name
        ),
        class = "inzlnk_spec"
    )
}
