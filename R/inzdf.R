#' iNZight data frame object
#'
#' This object allows the data to be either a standard R `data.frame` or
#' a connection to a database.
#'
#' TODO:
#' It is possible to specify a linking structure between multiple datasets,
#' and when variables are selected the dataset will be linked 'on-the-fly'.
#' This, when used with databases, will significantly reduce the size of data in memory.
#'
#' @param x a data.frame or db connection
#' @param name the name of the data
#' @param ... additional arguments passed to methods
#' @return an `inzdf` object
#' @md
#' @export
inzdf <- function(x, name, ...) {
    UseMethod("inzdf")
}

inzdf.tbl_df <- function(x, name, ...) {
    if (missing(name)) {
        if (is.null(attr(x, "name", exact = TRUE))) name <- deparse(substitute(x))
        else name <- attr(x, "name", exact = TRUE)
    }
    structure(
        tibble::as_tibble(x),
        class = c("inzdf_tbl_df", "inzdf", class(x)),
        name = name
    )
}

#' @rdname inzdf
#' @export
inzdf.data.frame <- function(x, name, ...) {
    inzdf(tibble::as_tibble(x), name = name)
}

#' @rdname inzdf
#' @export
inzdf.SQLiteConnection <- function(x, name = deparse(substitute(x)), ...) {
    structure(
        list(
            connection = x,
            type = "SQLite"
        ),
        class = c("inzdf_sqlite", "inzdf_db", "inzdf"),
        name = name
    )
}

#' @export
print.inzdf <- function(x, ...) {
    cat("Data:", attr(x, "name", exact = TRUE), "\n\n")
}

#' @export
print.inzdf_tbl_df <- function(x, ...) {
    NextMethod()

    print(tibble::as_tibble(x))
}

#' @export
print.inzdf_db <- function(x, ...) {
    NextMethod()

    tbls <- DBI::dbListTables(x$connection)
    if (length(tbls) == 0L) {
        cat("No tables\n")
        return()
    }

    if (length(tbls) > 1L) {
        cat("Tables:\n")
        cat(paste("  -", DBI::dbListTables(x$connection), collapse = "\n"))
        cat("\n\n")
    } else {
        print(dplyr::tbl(x$connection, tbls[1]))
    }
}

#' @export
`[.inzdf_db` <- function(x, i, j, table = DBI::dbListTables(x$connection)[1]) {
    e <- rlang::expr(dplyr::tbl(x$connection, !!table))

    if (!missing(j)) {
        e <- rlang::expr(
            !!rlang::enexpr(e) %>%
                dplyr::select(!!j)
        )
    }

    if (!missing(i)) {
        warning('row subsetting not supported')
    }

    # e
    eval(e)
}

get_tbl <- function(x, table = DBI::dbListTables(x)[1]) {
    dplyr::tbl(x$connection, DBI::dbListTables(x$connection)[1])
}

#' @inheritParams dplyr::select
#' @importFrom dplyr select
#' @export
select.inzdf_db <- function(.data, ..., table) {
    get_tbl(.data, table) %>%
        dplyr::select(...)
}

#' Filter
#' @name filter
#' @importFrom dplyr filter
#' @export
NULL

#' Filter inzdf
#' @inheritParams dplyr::filter
#' @param table name of the table to use, defaults to first in list
#' @param .preserve ignored
#' @importFrom dplyr filter
#' @export
#' @rdname filter
filter.inzdf_db <- function(.data, ..., table, .preserve = FALSE) {
    get_tbl(.data, table) %>%
        dplyr::filter(...)
}
