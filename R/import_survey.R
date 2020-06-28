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
                ids = if (is.null(spec$clusters)) 1 else spec$clusters,
                probs = spec$probs,
                strata = spec$strata,
                fpc = spec$fpc,
                nest = spec$nest,
                weights = spec$weights
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

    exp <- ~survey::svydesign(terms, data = .data)

    s <- spec$spec
    terms <- do.call(
        paste,
        c(
            lapply(names(s)[!sapply(s, is.null)],
                function(x) {
                    sprintf("%s = %s%s",
                        x,
                        ifelse(x %in% c("nest"), "", "~"),
                        s[[x]]
                    )
                }
            ),
            list(sep = ",")
        )
    )

    exp <- replaceVars(exp, terms = terms)
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
