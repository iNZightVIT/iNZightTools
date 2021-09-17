#' Read CSV with iNZight metadata
#'
#' This function will read a CSV file with iNZight metadata in the header.
#' This allows plain text CSV files to be supplied with additional comments
#' that describe the structure of the data to make import and
#' data handling easier.
#'
#' The main example is to define factor levels for an integer variable
#' in large data sets.
#'
#' @param file the plain text file with metadata
#' @param preview logical, if \code{TRUE} only the first 10 rows are returned
#' @param column_types optional column types
#' @param ... more arguments
#' @return a data frame
#' @author Tom Elliott
#' @export
read_meta <- function(file, preview = FALSE, column_types, ...) {
    if (!is.character(file)) stop("file must be a string")
    if (!file.exists(file)) stop("That file doesn't seem to exist.")

    ## fetch the metadata
    meta <- readMetaComments(file)
    if (is.null(meta)) {
        return(
            read_dlm(file,
                preview = preview,
                column_types = column_types,
                ...
            )
        )
    }

    ## fetch the first few rows of the data ...
    suppressMessages({
        dtop <- read_dlm(file,
            preview = TRUE,
            column_types = column_types,
            ...,
            progress = FALSE
        )
    })

    ## columns in meta not in dataset:
    mvars <- gsub("_missing", "", sapply(meta$columns, getname))
    dvars <- names(dtop)
    if (any(!mvars %in% dvars)) {
        stop("Some variables defined in metadata not in dataset: ",
            paste(mvars[!mvars %in% dvars], collapse = ", "))
    }

    ## use metadata to determine column types
    mtypes <- sapply(meta$columns, gettype, abbr = TRUE)
    names(mtypes) <- mvars
    ctypes <- paste(
        ifelse(dvars %in% mvars, mtypes[dvars], "?"),
        collapse = ""
    )

    ## read the data (strings remain as strings)
    data <- read_dlm(file,
        col_types = ctypes,
        preview = preview,
        convert.to.factor = FALSE,
        ...
    )

    ## Do this with mutate ()
    ## convert factors appropriately
    mutatestr <- c(
        lapply(meta$columns, function(c) {
            fnc <- c$fun(data[[getname(c)]], getname(c))
            if (!is.null(fnc))
                return(sprintf("%s = %s", getname(c, original = FALSE), fnc))
            if (rename(c))
                return(sprintf("%s = %s",
                    getname(c, original = FALSE),
                    getname(c)
                ))
            NULL
        }),
        lapply(dvars[!dvars %in% mvars], function(c) {
            if (is.character(data[[c]])) {
                cesc <- c
                if (grepl(" ", c))
                    cesc <- sprintf("`%s`", c)
                return(sprintf("%s = as.factor(%s)", cesc, cesc))
            }
            NULL
        })
    )

    ## remaining vars
    if (!all(sapply(mutatestr, is.null))) {
        mexp <- eval(parse(
            text = paste0(
                "~.dataset %>% dplyr::mutate(",
                paste(
                    mutatestr[!sapply(mutatestr, is.null)],
                    collapse = ",\n   "
                ),
                ")"
            )
        ))

        e <- new.env()
        e$.dataset <- data
        dcode <- paste(code(data), collapse = " ")
        data <- suppressWarnings(interpolate(mexp, "_env" = e))
        attr(data, "code") <- gsub(".dataset", dcode, code(data))
    }

    ## convert any Lists to binary matrices (within the df)
    if (any(sapply(data, class) == "list")) {
        list_vars <- names(data)[sapply(data, class) == "list"]
        new_cols <- sapply(list_vars,
            function(v) {
                lvls <- unique(unlist(data[[v]]))
                mc <- which(sapply(meta$columns, function(x) x$name == v))
                lbls <- if (length(mc) && !is.null(meta$columns[[mc]]$labels))
                    meta$columns[[mc]]$labels else NULL

                cnames <- paste(sep = "_", v, lvls)
                sprintf(
                    "tidyr::unnest(%s) %s%s
    dplyr::mutate(n = 1%s) %s%s
    tidyr::pivot_wider(
        names_from = %s,
        values_from = n,
        values_fill = 0,
        names_prefix = \"%s_\"
    )",
                    v, "%>%",
                    if (is.null(lbls$labels)) ""
                    else sprintf("
    dplyr::left_join(
        data.frame(
            %s = %s,
            labels = %s
        ),
        by = '%s'
    ) %s",
                        v,
                        capture.output(dput(lbls$labels)),
                        capture.output(dput(lbls$levels)),
                        v,
                        " %>%"
                    ),
                    if (is.null(lbls$labels)) ""
                    else sprintf(", %s = labels", v),
                    "%>%",
                    if (is.null(lbls$labels)) ""
                    else "
    dplyr::select(-labels) %>%",
                    v, v
                )
            }
        )

        mexp <- eval(parse(
            text = sprintf("~.dataset %s %s",
                "%>%", paste(new_cols, collapse = " %>% ")
            )
        ))

        e <- new.env()
        e$.dataset <- data
        dcode <- paste(code(data), collapse = " ")
        data <- suppressWarnings(interpolate(mexp, "_env" = e))
        attr(data, "code") <- gsub(".dataset", dcode, code(data))
    }


    attr(data, "name") <- meta$title
    attr(data, "description") <- meta$desc

    data
}


readMetaComments <- function(file) {
    con <- file(file, open = "r")
    meta <- character()
    while (grepl("^#", x <- readLines(con, n = 1)))
        if (grepl("^#'", x))
            meta <- c(meta, trimws(gsub("^#'", "", x)))
    close(con)

    if (length(meta) == 0)
        return(NULL)

    md <- list(
        title = tools::file_path_sans_ext(basename(file)),
        desc = NULL,
        columns = list()
    )

    ## Grab the title, if present - always the first line of metadata
    if (!grepl("^@", meta[1])) {
        md$title = trimws(gsub("^@", "", meta[1]))
        meta <- meta[-1]
    }

    ## Remove any empty lines between title description
    while (meta[1] == "")
        meta <- meta[-1]

    ## Grab the description - all content between the title and first @
    desc.end <- grep("^@", meta)[1] - 1
    if (desc.end > 0) {
        desc <- meta[1:desc.end]
        ## drop off the last empty line
        if (desc[length(desc)] == "")
            desc <- desc[-length(desc)]
        ## add paragraph breaks
        desc[desc == ""] <- "\n\n"
        ## add space at END of each line, but collapse with no space
        ## so new lines are flush
        md$desc <- trimws(paste(trimws(desc), " ", collapse = ""))
        meta <- meta[- (1:desc.end)]
    }

    ## check remaining lines commence with a @; drop any that don't
    if (any(!grepl("^@", meta))) {
        warning(paste(
            "All lines after title and description should start with a @.",
            "Skipping those that don't"
        ))
        meta <- meta[grepl("^@", meta)]
    }

    ## process each column
    md$columns <- processLines(meta)
    md
}

processLines <- function(metadata) {
    lapply(metadata, processLine)
}

processLine <- function(x) {
    txt <- cleanstring(x)
    txt <- strsplit(x, " ")[[1]]

    ## now x is a vector of components
    if (length(txt) == 1)
        stop(
            "Invalid metadata: ", x,
            ". Needs at least a type and a column name"
        )

    type <- txt[1]
    txt <- txt[-1]

    ## use s3 methods to provide extensible method for processing metadata
    ## drop the type as its redundant from here ...
    class(txt) <- sprintf("inz.meta.%s", gsub("^@", "", type))
    .processLine(txt)
}

cleanstring <- function(x) {
    ## return a cleaned meta data string

    ## how about dealing with quotes?

    ## removing additional spaces around: '=', '@', '+', '-', '\', ','
    x <- gsub('\\s*([,@\\[\\=\\+\\-\\\\])\\s*','\\1', x, perl = TRUE)

    x
}


.processLine <- function(x) UseMethod(".processLine")

.processLine.default <- function(x) {
    warning("Unknown type: ", class(x))
    return(NULL)
}

.processLine.inz.meta.numeric <- function(x) {
    ## The name of the variable
    vname <- x[1]

    ## some other calculations? perhaps ...
    na_codes <- x[grepl("^na=", x)]
    na_codes <- if (length(na_codes) == 0L) NULL
        else strsplit(gsub("^na=", "", na_codes), ",")[[1]]

    if (is.null(na_codes) && any(grepl("^na\\[", x))) {
        # alternatively, NA codes can be specifed for an additional column
        na_codes <- extract_levels(x[grepl("^na\\[", x)])
        vname <- glue::glue("{vname}_missing")
    }

    fn <- sprintf(
        "function(x, vname) {
            %s
            if (is.numeric(x)) return(NULL)
            sprintf('as.numeric(%s)', vname)
        }",
        if (!is.null(na_codes)) {
            Xa <- "if (is.numeric(x)) vname else sprintf(\"as.numeric(%s)\", gsub(\"_missing\", \"\", vname))"
            if (is.list(na_codes)) {
                # case_when(x == 88 ~ 'Refused', x == 99 ~ 'Dont_Know', TRUE ~ 'observed'), c = ifelse(c_missing == 'observed', c, NA)
                codes <- paste0(gsub("_missing", "", vname), " == ", na_codes$labels, " ~ \"", na_codes$levels, "\"", collapse = ", ")
                glue::glue(
                    "return(sprintf(
                        'dplyr::case_when({codes},
                            TRUE ~ \"observed\"
                        ),
                        %s = ifelse(%s == \"observed\", %s, NA)',
                    gsub(\"_missing\", \"\", vname), vname, {Xa}))"
                )
            } else {
                Xin <- "IN_"
                Xb <- capture.output(dput(as.numeric(na_codes)))
                Xc <- Xa
                glue::glue(
                    "return(sprintf(\"ifelse(%s %s {Xb}, NA, %s)\",
                        {Xa},
                        \"{Xin}\",
                        {Xc}
                    ))"
                )
            }
        } else {
            ""
        },
        "%s"
    )
    fn <- eval(parse(
        text =
            gsub("IN_", "%in%", fn)
    ))

    ## and return a meta object
    metaFun(
        type = "numeric",
        name = vname,
        fun = fn
    )
}

extract_levels <- function(vname) {
    out <- list(levels = NULL, labels = NULL, vname = vname)

    if (!grepl("\\[", vname)) return(out)

    f <- strsplit(vname, "\\[")[[1]]
    vname <- f[1]
    lvlstr <- trimws(strsplit(gsub("\\]", "", f[2]), ",")[[1]])
    labels <- levels <- character(length(lvlstr))
    for (i in seq_along(lvlstr)) {
        lvl <- strsplit(lvlstr[i], "=")[[1]]
        levels[i] <- lvl[1]
        labels[i] <- lvl[length(lvl)]
    }
    levels <- iconv(levels, from = "UTF-8", to = "ASCII//TRANSLIT")

    list(levels = levels, labels = labels, vname = vname)
}

.processLine.inz.meta.factor <- function(x) {
    vname <- x[1]

    ## extract levels=labels (level in output, label in dataset)
    ll <- extract_levels(vname)
    levels <- ll$levels
    labels <- ll$labels
    vname <- ll$vname

    if (is.null(levels)) {
        fun <- "function(x, vname) {
             if (is.factor(x)) return(NULL)
             sprintf('as.factor(%s)', vname)
        }"
    } else {
        fun <- "function(x, vname) {
            sprintf(
                \"forcats::fct_relevel(forcats::fct_collapse(%s, %s), '%s')\",
                ifelse(inherits(x, 'factor'), vname,
                       sprintf('as.factor(%s)', vname)),
                paste(levels, \" = c('\",
                      gsub(\"|\", \"', '\", labels, fixed = TRUE),
                      \"')\",
                      sep = \"\", collapse = \", \"),
                paste(levels, collapse = \"', '\")
            )
            }"
    }

    metaFun(
        type = "factor",
        name = vname,
        fun = eval(parse(text = fun))
    )
}

.processLine.inz.meta.multi <- function(x) {
    # a method for parsing multiple-response columns,
    # where choices are separated by some separator
    # e.g., # @multi tech sep=;

    ## The name of the variable
    vname <- x[1]
    ll <- extract_levels(vname)
    vname <- ll$vname

    sep <- x[grepl("^sep=", x)]
    if (length(sep) == 0L) sep <- "," else sep <- gsub("^sep=", "", sep)

    fun <-
        "function(x, vname)
            sprintf(
                \"stringr::str_split(%s, '%s')\",
                vname,
                sep
            )
        "

    metaFun(
        type = "multi",
        name = vname,
        fun = eval(parse(text = fun)),
        labels = ll
    )
}
