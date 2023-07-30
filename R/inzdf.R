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

#' @rdname inzdf
#' @export
inzdf.tbl_df <- function(x, name, ...) {
    if (missing(name)) {
        if (is.null(attr(x, "name", exact = TRUE))) {
            name <- deparse(substitute(x))
        } else {
            name <- attr(x, "name", exact = TRUE)
        }
    }
    structure(
        tibble::as_tibble(x),
        db = list(
            connection = NA_character_,
            schema = NULL,
            type = NA_character_,
            var_attrs = list()
        ),
        class = c("inzdf_tbl_df", "inzdf", class(x)),
        name = name,
        dictionary = NULL
    )
}

#' @rdname inzdf
#' @export
inzdf.data.frame <- function(x, name, ...) {
    inzdf(tibble::as_tibble(x), name = name)
}

#' @param schema a list specifying the schema of the database (used for linking)
#' @param var_attrs nested list of variables attributes for each table > variable
#' @param dictionary an inzdict object
#' @param keep_con if `TRUE` data will remain in DB (use for very large data)
#' @rdname inzdf
#' @export
inzdf.SQLiteConnection <- function(x,
                                   name = deparse(substitute(x)),
                                   schema = NULL,
                                   var_attrs = list(),
                                   dictionary = NULL,
                                   keep_con = FALSE,
                                   ...) {
    if (!requireNamespace("dbplyr", quietly = TRUE)) {
        stop("dbplyr is required to use inzdf with databases")
    }

    # TODO: add col types to schema (if missing)
    x <- structure(
        list(),
        db = list(
            connection = x,
            schema = schema,
            type = "SQLite",
            var_attrs = var_attrs
        ),
        class = c("inzdf_sqlite", "inzdf_db", "inzdf"),
        name = name,
        row.names = NA_integer_,
        dictionary = dictionary
    )
    if (keep_con) {
        return(x)
    }

    x <- as_tibble(x)
    structure(
        x,
        db = list(
            connection = NA_character_,
            schema = NULL,
            type = NA_character_,
            var_attrs = list()
        ),
        class = c("inzdf_tbl_df", "inzdf", class(x)),
        name = name,
        dictionary = dictionary
    )
}

con <- function(x) {
    db <- attr(x, "db", exact = TRUE)
    if (is.null(db)) {
        return(NULL)
    }
    db$connection
}
schema <- function(x) {
    db <- attr(x, "db", exact = TRUE)
    if (!is.null(db)) {
        return(db$schema)
    }
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
    e <- rlang::expr(get_tbl(x, !!table))

    if (missing(j) && !missing(i)) j <- i

    if (!missing(j)) {
        e <- rlang::expr(
            !!rlang::enexpr(e) %>%
                dplyr::select(!!j)
        )
    }

    if (!missing(i) && !missing(j)) {
        warning("row subsetting not supported")
    }

    d <- eval(e)
    structure(
        d,
        class = c("inzdf_lazydb", class(d)),
        data = x
    )
}

#' @export
`[[.inzdf_db` <- function(x, i, exact = TRUE, stringsAsFactors = TRUE) {
    if (is.numeric(i)) {
        vari <- names(x)[i]
    } else {
        vari <- i
    }

    z <- get_tbl(x, vars = vari)

    z <- dplyr::pull(z, vari) %>%
        set_attributes(x, var = vari)

    z
}

get_tbl <- function(x, table = NULL, include_links = TRUE, vars) {
    if (!requireNamespace("dbplyr", quietly = TRUE)) {
        stop("dbplyr is required to use inzdf with databases")
    }

    if (is.null(table)) {
        table <- if (is.null(schema(x))) {
            DBI::dbListTables(con(x))[1]
        } else {
            names(schema(x))[1]
        }
    }

    if (!include_links ||
        is.null(schema(x)) ||
        is.null(schema(x)[[table]]) ||
        is.null(schema(x)[[table]]$links_to)
    ) {
        d <- dplyr::tbl(con(x), table)
        if (!missing(vars)) {
            tbl_vars <- colnames(d)
            d <- dplyr::select(d, vars[vars %in% tbl_vars])
        }
        return(d)
    }

    # do magic linking:
    links <- schema(x)[[table]]$links_to
    d <- dplyr::tbl(con(x), table)
    if (!missing(vars)) {
        tbl_vars <- colnames(d)
        link_vars <- do.call(c, lapply(links, function(x) c(names(x), as.character(x))))
        vars <- c(vars, as.character(link_vars))
        d <- dplyr::select(d, vars[vars %in% tbl_vars])
    }

    for (link in names(links)) {
        d <- link_table(
            d,
            get_tbl(x, link, include_links = TRUE, vars = vars),
            links[[link]]
        )
    }

    d
}

link_table <- function(data, table, schema, join = "left") {
    join_fun <- eval(parse(text = sprintf("dplyr::%s_join", join)))

    if (is.null(schema)) {
        return(join_fun(data, table, copy = TRUE))
    }

    join_fun(data, table, by = schema, copy = TRUE)
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
    x <- get_tbl(.data, table) %>%
        dplyr::select(...)
    structure(
        x,
        class = c("inzdf_lazydb", class(x)),
        data = .data
    )
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
    if (isTRUE(as.logical(Sys.getenv("DEBUG")))) print("--as tibble")

    d <- get_tbl(x, table) %>%
        dplyr::collect() %>%
        set_attributes(x)

    attr(d, "name") <- attr(x, "name", exact = TRUE)
    # if (stringsAsFactors) {
    #     for (col in names(d)) {
    #         if (is.character(d[[col]])) d[[col]] <- as.factor(d[[col]])
    #     }
    # }
    d
}

#' @export
as_tibble.inzdf_lazydb <- function(x, ...) {
    d <- dplyr::collect(x) %>%
        set_attributes(attr(x, "data", exact = TRUE))
    attr(d, "name") <- attr(attr(x, "data", exact = TRUE), "name", exact = TRUE)
    d
}

set_attributes <- function(x, from, var = names(x)) {
    db <- attr(from, "db")
    if (is.null(db) || is.null(db$var_attrs) || length(db$var_attrs) == 0) {
        return(x)
    }

    attrs <- db$var_attrs
    if (is.data.frame(x)) {
        for (v in names(x)) {
            x[[v]] <- set_var_attributes(x[[v]], v, attrs)
        }
    } else {
        x <- set_var_attributes(x, var, attrs)
    }

    x
}

set_var_attributes <- function(x, var, attrs) {
    tbl_names <- lapply(attrs, names)

    # TODO: this needs to be moved to 'get_tbl' to be more precise ...
    # find a column with the same name
    var <- as.character(var)
    vi <- sapply(tbl_names, function(n) var %in% n)
    if (!any(vi)) {
        return(x)
    }

    mi <- as.integer(which(vi)[1])
    xa <- attrs[[mi]][[var]]

    levels <- if (is.null(xa$levels)) unique(x) else xa$levels

    # set the class
    if (is.character(x) && any(xa$class == "factor")) x <- factor(x, levels = levels)
    do.call(structure, c(list(x), xa, list(table = names(tbl_names)[mi])))
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
    head(get_tbl(x), ...) %>%
        dplyr::collect() %>%
        set_attributes(x)
}

#' @export
dim.inzdf_db <- function(x) {
    c(
        dplyr::collect(dplyr::count(get_tbl(x)))$n,
        length(names(x))
    )
}

# prevent CRAN from complaining about not importing
# from dbplyr (it is required by dplyr)
dummy_function_to_use_dbplyr <- function() {
    if (!requireNamespace("dbplyr", quietly = TRUE)) {
        stop("DBI is required to use inzdf with databases")
    }
    dbplyr::as.sql
    invisible()
}
