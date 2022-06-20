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
        db = list(
            connection = NA_character_,
            schema = NULL,
            type = NA_character_
        ),
        class = c("inzdf_tbl_df", "inzdf", class(x)),
        name = name
    )
}

#' @rdname inzdf
#' @export
inzdf.data.frame <- function(x, name, ...) {
    inzdf(tibble::as_tibble(x), name = name)
}

#' @param schema a list specifying the schema of the database (used for linking)
#' @rdname inzdf
#' @export
inzdf.SQLiteConnection <- function(x, name = deparse(substitute(x)), schema = NULL, ...) {
    # TODO: add col types to schema (if missing)
    structure(
        list(),
        db = list(
            connection = x,
            schema = schema,
            type = "SQLite"
        ),
        class = c("inzdf_sqlite", "inzdf_db", "inzdf"),
        name = name,
        row.names = NA_integer_
    )
}

con <- function(x) {
    db <- attr(x, "db", exact = TRUE)
    if (is.null(db)) return(NULL)
    db$connection
}
schema <- function(x) {
    db <- attr(x, "db", exact = TRUE)
    if (!is.null(db)) return(db$schema)
    NULL
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

    tbls <- DBI::dbListTables(con(x))
    if (length(tbls) == 0L) {
        cat("No tables\n")
        return()
    }

    cat("Tables:\n")
    cat(paste("  -", DBI::dbListTables(con(x)), collapse = "\n"))
    cat("\n\n")
}

#' @export
`[.inzdf_db` <- function(x, i, j, table = DBI::dbListTables(con(x))[1]) {
    e <- rlang::expr(dplyr::tbl(con(x), !!table))

    if (missing(j) && !missing(i)) j <- i

    if (!missing(j)) {
        e <- rlang::expr(
            !!rlang::enexpr(e) %>%
                dplyr::select(!!j)
        )
    }

    if (!missing(i) && !missing(j)) {
        warning('row subsetting not supported')
    }

    # e
    eval(e)
}

#' @export
`[[.inzdf_db` <- function(x, i, exact = TRUE, stringsAsFactors = TRUE) {
    z <- get_tbl(x) %>%
        dplyr::pull(!!i)
    if (stringsAsFactors && is.character(z)) z <- as.factor(z)
    z
}

get_tbl <- function(x, table = NULL, include_links = TRUE) {
    if (is.null(table))
        table <- if (is.null(schema(x))) DBI::dbListTables(con(x))[1]
            else names(schema(x))[1]

    if (!include_links ||
        is.null(schema(x)) ||
        is.null(schema(x)[[table]]) ||
        is.null(schema(x)[[table]]$links_to)
    ) {
        return(
            dplyr::tbl(con(x), table)
        )
    }

    # do magic linking:
    links <- schema(x)[[table]]$links_to
    d <- dplyr::tbl(con(x), table)

    for (link in names(links)) {
        d <- link_table(d,
            get_tbl(x, link, include_links = TRUE),
            links[[link]]
        )
    }

    d
}

link_table <- function(data, table, schema, join = "inner") {
    join_fun <- eval(parse(text = sprintf("dplyr::%s_join", join)))

    if (is.null(schema))
        return(join_fun(data, table))

    join_fun(data, table, by = schema)
}

#' Select
#' @name select
#' @importFrom dplyr select
#' @export
NULL

#' @inheritParams dplyr::select
#' @importFrom dplyr select
#' @export
select.inzdf_db <- function(.data, ..., table = NULL) {
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
filter.inzdf_db <- function(.data, ..., table = NULL, .preserve = FALSE) {
    get_tbl(.data, table) %>%
        dplyr::filter(...)
}

#' @export
as_tibble.inzdf_db <- function(x, table = NULL, ..., stringsAsFactors = TRUE) {
    print("--as tibble")
    d <- get_tbl(x, table) %>%
        dplyr::collect()
    attr(d, "name") <- attr(x, "name", exact = TRUE)
    if (stringsAsFactors) {
        for (col in names(d)) {
            if (is.character(d[[col]])) d[[col]] <- as.factor(d[[col]])
        }
    }
    d
}

#' @export
as.data.frame.inzdf_db <- function(x, row.names = NULL, optional = FALSE, table = NULL, ...) {
    as.data.frame(as_tibble(x, table))
}

#' @export
names.inzdf_db <- function(x) {
    colnames(get_tbl(x))
}

#' @importFrom utils head
#' @export
head.inzdf_db <- function(x, ...) {
    head(get_tbl(x), ...) %>% dplyr::collect()
}

#' @export
dim.inzdf_db <- function(x) {
    c(
        dplyr::collect(dplyr::count(get_tbl(x)))$n,
        length(names(x))
    )
}
