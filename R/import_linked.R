#' Import linked data into an `inzdf` object
#'
#' @param x a linked specification file or vector of data set paths
#' @param schema a list describing the schema/relationships between the files
#' @param con a database connection to load the linked data into
#' @param name the name of the data set collection
#' @param keep_con if `TRUE` data will remain in DB (use for very large data)
#' @param progress either `TRUE` or `FALSE` to enable/disable the default progress bar, or a list of three functions to `x <- create(from, to)`, `set(x, i)`, and `destroy(x)` a progress bar.
#' @param ... additional arguments passed to data reading function `smart_read()`
#'
#' @return an `inzdf` object
#' @md
#' @export
load_linked <- function(x, schema, con,
                        name = ifelse(missing(con),
                            deparse(substitute(x)),
                            deparse(substitute(con))
                        ),
                        keep_con = FALSE,
                        progress = FALSE,
                        ...) {
    fdir <- NULL
    if (!inherits(x, "inzlnk_spec")) {
        if (length(x) == 1L && tools::file_ext(x) == "inzlnk") {
            fdir <- dirname(x)
            x <- read_link_spec(x)
        } else {
            x <- link_spec(x, schema, name = name)
        }
    }

    if (is.null(x$schema)) stop("Need to specify a schema")

    if (!is.null(x$dictionary)) {
        dict_path <- x$dictionary$file
        if (!is.null(fdir) && !grepl("^https?://", dict_path)) {
            if (!file.exists(dict_path)) {
                dict_path <- file.path(fdir, dict_path)
            }
            if (!file.exists(dict_path)) {
                stop("cannot find", dict_path)
            }
        }
        dict_path <- normalizePath(dict_path)
        x$dictionary$file <- dict_path
        dict <- do.call(read_dictionary, x$dictionary)
        x$dictionary <- dict
    }

    if (missing(con)) {
        if (is.logical(progress)) {
            if (progress) {
                progress <- list(
                    create = function(from, to) utils::txtProgressBar(from, to, style = 3L),
                    set = function(x, i) utils::setTxtProgressBar(x, i),
                    destroy = function(x) close(x)
                )
            } else {
                progress <- NULL
            }
        }

        if (!is.null(progress)) try(pb <- progress$create(0, length(x$files)), silent = TRUE)
        data <- lapply(seq_along(x$files), function(i) {
            fname <- names(x$files)[i]
            f <- x$files[[i]]
            if (!is.null(fdir) && !grepl("^https?://", f)) {
                if (!file.exists(f)) {
                    f <- file.path(fdir, f)
                }
                if (!file.exists(f)) {
                    stop("cannot find", f)
                }
            }
            f <- normalizePath(f)
            d <- smart_read(f, ...)
            if (!is.null(x$dictionary)) {
                d <- apply_dictionary(d, x$dictionary)
            }
            for (c in names(d)) attr(d[[c]], "table") <- fname
            if (!is.null(progress)) progress$set(pb, i)
            d
        })

        names(data) <- names(x$files)
        dat <- link_data(data, schema = x$schema)

        dat <- structure(
            dat,
            db = list(
                connection = NA_character_,
                schema = NULL,
                type = NA_character_,
                var_attrs = list()
            ),
            class = c("inzdf_tbl_df", "inzdf", class(dat)),
            name = name,
            dictionary = x$dictionary
        )

        if (!is.null(progress)) progress$destroy(pb)

        return(dat)
    }

    var_attrs <- lapply(
        names(x$files),
        function(f) {
            fpath <- x$files[f]
            if (!is.null(fdir) && !grepl("^https?://", fpath) && !grepl("/", fpath)) {
                fpath <- file.path(fdir, fpath)
            }
            d <- smart_read(fpath, ...)
            if (!is.null(x$dictionary)) {
                d <- apply_dictionary(d, x$dictionary)
            }
            vf <- lapply(d, function(x) {
                utils::modifyList(list(class = class(x)), as.list(attributes(x)))
            })
            on.exit(rm(d))
            DBI::dbWriteTable(con, f, d)
            vf
        }
    )
    names(var_attrs) <- names(x$files)

    inzdf(con,
        name = name,
        schema = x$schema,
        var_attrs,
        dictionary = x$dictionary,
        keep_con = keep_con
    )
}

link_data <- function(x, table, schema) {
    if (missing(table)) table <- names(schema)[1]

    links <- schema[[table]]$links_to
    d <- x[[table]]

    # convert linking cols to character
    link_cols <- do.call(
        c,
        lapply(links, function(l) if (is.null(names(l))) l else names(l))
    )
    for (c in as.character(link_cols)) {
        if (is.factor(d[[c]])) d[[c]] <- as.character(d[[c]])
    }

    for (link in names(links)) {
        ld <- link_data(x, link, schema)
        for (c in as.character(links[[link]])) {
            if (is.factor(ld[[c]])) ld[[c]] <- as.character(ld[[c]])
        }

        d <- link_table(d, ld, links[[link]])
    }

    d
}

table_spec <- function(x) {
    x$links_to <- lapply(x$links_to, unlist)
    x
}

read_link_spec <- function(x, name = deparse(substitute(x))) {
    if (!requireNamespace("yaml", quietly = TRUE)) {
        stop("Please install the suggested package: 'yaml'") # nocov
    }
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
