#' Import survey information from a file
#'
#' The survey information should be in DCF format, with fields
#' corresponding to survey design components. For example,
#' ```
#' strata: strata_var
#' clusters: cluster_var
#' weights: wt_var
#' ```
#'
#' Additionally, the information can contain a `file` specification
#' indicating the path to the data, which will be imported using
#' `iNZightTools::smart_read` if it exists in the same directory
#' as `file`.
#'
#' @param file the file containing survey information (see Details)
#' @param data optional, if supplied the survey object will be created with the supplied data.
#'        Can be either a data.frame-like object, or a path to a data set which
#'        will be imported using `iNZightTools::smart_read`.
#' @return a `inzsvyspec` object containing the design parameters and, if data supplied,
#'         the created survey object
#' @author Tom Elliott
#' @export
#' @md
import_survey <- function(file, data) {
    spec <- as.data.frame(read.dcf(file), stringsAsFactors = FALSE)

    svyspec <- structure(
        list(
            spec = list(
                ids =
                    if ("clusters" %in% names(spec) && !is.null(spec$clusters)) spec$clusters
                    else if ("ids" %in% names(spec) && !is.null(spec$ids)) spec$ids
                    else 1,
                probs = spec$probs,
                strata = spec$strata,
                fpc = spec$fpc,
                nest = spec$nest,
                weights = spec$weights,
                repweights = spec$repweights,
                type = spec$type,
                scale = spec$scale,
                rscales = spec$rscales,
                ## this will become conditional on what fields are specified
                svy_type = ifelse("repweights" %in% names(spec), "replicate", "survey")
            )
        ),
        class = "inzsvyspec"
    )

    if (!is.null(spec$data)) {
        data <- file.path(dirname(file), spec$data)
        if (file.exists(data))
            data <- smart_read(data)
    } else if (!missing(data)) {
        if (is.character(data) && file.exists(data)) {
            data <- smart_read(data)
        }
    }

    if (missing(data) || !is.data.frame(data)) return(svyspec)

    make_survey(data, svyspec)
}

#' Make a survey object
#'
#' Construct a survey object from a data set and an `inzsvyspec` object.
#'
#' @param .data a data.frame
#' @param spec a `inzsvyspec` object
#' @return a `inzsvyspec` object with the survey design loaded
#' @author Tom Elliott
#' @export
#' @md
make_survey <- function(.data, spec) {
    mc <- match.call()
    dataname <- mc$.data

    type <- spec$spec$svy_type
    exp <- switch(type,
        "replicate" = ~survey::svrepdesign(terms, data = .data),
        "survey" = ~survey::svydesign(terms, data = .data)
    )


    s <- spec$spec
    s$svy_type <- NULL
    fmla_args <- c("ids", "probs", "strata", "fpc", "weights")
    str_args <- c("type")

    if (type == "replicate") {
        s <- s[names(s) %in% c("weights", "repweights", "type", "scale", "rscales")]
        # is repweights a formula or string?
        split <- trimws(strsplit(s$repweights, "+", fixed = TRUE)[[1]])
        if (all(split %in% names(.data))) {
            # a formula
            fmla_args <- c(fmla_args, "repweights")
        } else {
            # string/something else ...
            str_args <- c(str_args, "repweights")
        }
    }

    if (type == "survey") {
        s <- s[names(s) %in% c("ids", "probs", "strata", "fpc", "nest", "weights")]
    }

    terms <- do.call(
        paste,
        c(
            lapply(names(s)[!sapply(s, is.null)],
                function(x) {
                    sprintf("%s = %s%s%s",
                        x,
                        ifelse(x %in% fmla_args,
                            "~",
                            ifelse(x %in% str_args, "\"", "")
                        ),
                        s[[x]],
                        ifelse(x %in% str_args, "\"", "")
                    )
                }
            ),
            list(sep = ", ")
        )
    )

    exp <- replaceVars(exp, terms = terms)
    spec$data <- .data
    spec$design <- interpolate(exp, .data = dataname)
    spec
}

#' Print iNZight Survey Spec
#'
#' @param x a `inzsvyspec` object
#' @param ... additional arguments, ignored
#' @author Tom Elliott
#' @md
#' @export
print.inzsvyspec <- function(x, ...) {
    cat("Survey design specification:\n")
    s <- x$spec
    lapply(names(s),
        function(y) {
            if (is.null(s[[y]])) return()
            cat(sprintf(" * %s: %s\n", y, s[[y]]))
        }
    )

    cat("\nDesign object: ")
    if (is.null(x$design)) {
        cat("empty\n")
    } else {
        cat("\n")
        print(x$design)
    }
}
