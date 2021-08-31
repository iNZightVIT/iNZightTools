#' Import survey information from a file
#'
#' The survey information should be in TOML format, with fields
#' corresponding to survey design components. For example,
#' ```
#' strata = strata_var
#' clusters = cluster_var
#' weights = wt_var
#' ```
#'
#' For replicate weight designs, vectors (if necessary) are declared with
#' square brackets, like so:
#' ```
#' repweights = ['w01', 'w02', 'w03', 'w04', ..., 'w20']
#' ```
#' although this would be better expressed using a regular expression,
#' ```
#' repweights = '^w[0-2]'
#' ```
#' which matches all variables starting with a `w` followed by digits between 0 and 2 (inclusive).
#'
#' Additionally, the information can contain a `file` specification
#' indicating the path to the data, which will be imported using
#' `iNZightTools::smart_read` if it exists in the same directory
#' as `file`, or alternatively a URL to a data file that will be downloaded.
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
    # spec <- as.data.frame(read.dcf(file), stringsAsFactors = FALSE)
    spec <- RcppTOML::parseTOML(file)

    no_scale_types <- c("BRR", "ACS", "successive-difference", "JK2")
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
                reptype = spec$reptype,
                scale =
                    if (is.null(spec$reptype) || spec$reptype %in% c(no_scale_types)) {
                        NULL
                    } else {
                        spec$scale
                    },
                rscales =
                    if (is.null(spec$reptype) || spec$reptype %in% c(no_scale_types)) {
                        NULL
                    } else {
                        as.numeric(spec$rscales)
                    },
                ## this will become conditional on what fields are specified
                type = ifelse("repweights" %in% names(spec), "replicate", "survey"),
                calibrate = spec$calibrate
            )
        ),
        class = "inzsvyspec"
    )

    if (!is.null(spec$data)) {
        if (grepl("^https?://", spec$data)) {
            data <- spec$data
        } else {
            data <- file.path(dirname(file), spec$data)
            if (!file.exists(data)) data <- NULL
        }
    }

    if (!missing(data) && !is.null(data) && is.character(data)) {
        if (file.exists(data) || grepl("^https?://", data)) {
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

    type <- spec$spec$type
    exp <- switch(type,
        "replicate" = ~survey::svrepdesign(terms, data = .data),
        "survey" = ~survey::svydesign(terms, data = .data)
    )

    s <- spec$spec
    s$type <- NULL
    fmla_args <- c("ids", "probs", "strata", "fpc", "weights")
    str_args <- c("type")

    if (type == "replicate") {
        s <- s[names(s) %in% c("weights", "repweights", "reptype", "scale", "rscales")]
        if (is.character(s$repweights) && length(s$repweights) > 1L)
            s$repweights <- paste(s$repweights, collapse = " + ")
        # is repweights a formula or string?
        split <- trimws(strsplit(s$repweights, "+", fixed = TRUE)[[1]])
        if (all(split %in% names(.data))) {
            # a formula
            fmla_args <- c(fmla_args, "repweights")
        } else {
            # string/something else ...
            str_args <- c(str_args, "repweights")
        }
        if (is.null(s$rscales)) {
            s$rscales <- NULL
        } else {
            if (all(diff(s$rscales) == 0)) s$rscales <- s$rscales[1]
            s$rscales <- paste(capture.output(dput(s$rscales)), collapse = "")
        }
        s$type <- s$reptype
        s$reptype <- NULL
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

    if (!is.null(spec$spec$calibrate)) {
        cal <- spec$spec$calibrate
        # put cal into a more useful format
        vnames <- names(cal)
        pop.totals <- do.call(c,
            lapply(seq_along(vnames),
                function(i) {
                    x <- cal[[i]]
                    z <- paste0(vnames[[i]], names(x))
                    z[1] <- "(Intercept)"
                    x <- as.numeric(x)
                    x[1] <- sum(x)
                    names(x) <- z
                    if (i > 1L) x <- x[-1]
                    x
                }
            )
        )

        cal_exp <- ~survey::calibrate(.design, ~.vars, .totals)
        cal_exp <- replaceVars(cal_exp,
            .vars = paste(vnames, collapse = " + ")
        )
    }

    spec$data <- .data
    spec$design <- interpolate(exp, .data = dataname)
    if (!is.null(spec$spec$calibrate)) {
        # calibrate design:
        design_obj <- spec$design
        spec$design <- (function() {
            interpolate(cal_exp,
                .totals = pop.totals,
                .design = ~design_obj
            )
        })()
    }
    spec
}

#' Parse survey to survey spec
#'
#' @param x an object which can be converted to a survey spec (e.g., survey.design)
#' @return an `inzsvydesign` file
#' @author Tom Elliott
#' @md
#' @export
as_survey_spec <- function(x) UseMethod("as_survey_spec")

#' @describeIn as_survey_spec Method for survey.design objects
#' @export
as_survey_spec.survey.design <- function(x) {
    get_arg <- function(x, arg) {
        x <- x$call
        orNULL(x[[arg]], as.character(x[[arg]])[2])
    }
    spec <- list(
        spec = list(
            ids = get_arg(x, 2),
            probs = get_arg(x, "probs"),
            strata = get_arg(x, "strata"),
            fpc = get_arg(x, "fpc"),
            nest = get_arg(x, "nest"),
            weights = get_arg(x, "weights")
        ),
        data = x$variables,
        design = x
    )
    class(spec) <- "inzsvyspec"
    spec
}

#' as_survey method
#'
#' @importFrom srvyr as_survey
#' @name as_survey
#' @rdname as_survey.inzsvyspec
#' @export
NULL

#' Coerce to survey design
#'
#' Coerce an object to a survey design by extracting the survey object
#'
#' @param .data an `inzsvyspec` object
#' @param ... additional arguments, ignored
#' @return a survey design object
#' @export
#' @md
as_survey.inzsvyspec <- function(.data, ...) {
    .data$design
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
            if (y == "calibrate") {
                cat(sprintf(" * %s: %s\n",
                    y,
                    paste(names(s[[y]]), collapse = " + ")
                ))
            } else {
                cat(sprintf(" * %s: %s\n", y, s[[y]]))
            }
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
