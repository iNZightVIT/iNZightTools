#' Fit models
#'
#' Wrapper function for `lm`, `glm`, and `svyglm`.
#'
#' @param y character string representing the response,
#' @param x character string of the explanatory variables,
#' @param data name of the object containing the data.
#' @param family gaussian, binomial, poisson (so far, no others will be added)
#' @param link the link function to use
#' @param design data design specification.
#'        one of 'simple', 'survey' or 'experiment'
#' @param svydes  a vector of arguments to be passed to the svydesign function,
#'        excluding data (defined above)
#' @param ... further arguments to be passed to lm, glm, svyglm,
#'        such as offset, etc.
#' @return A model call formula (using lm, glm, or svyglm)
#' @author Tom Elliott
#' @import survey
#' @export
fitModel <- function(y, x, data,
                     family = "gaussian",
                     link = switch(family,
                         "gaussian" = "gaussian",
                         "binomial" = "logit",
                         "poisson" = "log"
                     ),
                     design = "simple",
                     svydes = NA,
                     ...) {

    if (missing(x) || length(x) == 0 || x == "") x <- 1
    Formula <- paste(y, x, sep = " ~ ")
    dat <- paste("data", data, sep = " = ")
    fam <- paste("family", family, sep = " = ")
    if (family == "binomial" && link != "logit") {
        fam <- sprintf("%s(link = \"%s\")", fam, link)
    }

    # Deal with extra arguments (eg. weights, offset ...)
    xarg <- list(...)
    xargs <- paste(names(xarg), xarg, sep = " = ", collapse = ", ")

    if (design == "simple") {
        # simple IID data:
        if (family == "gaussian") {
            # Simple linear regression model:
            args <- paste(Formula, dat, sep = ", ")
            if (xargs != "")
                args <- paste(args, xargs, sep = ", ")
            call <- paste("lm(", args, ")", sep = "")
        } else {
            # general linear model:
            args <- paste(Formula, dat, fam, sep = ", ")
            if (xargs != "")
                args <- paste(args, xargs, sep = ", ")
            call <- paste("glm(", args, ")", sep = "")
        }
    } else if (design == "survey") {
        # complex survey design:
        # set up the svyglm function call
        args <- paste(Formula, fam, "design = svy.design", sep = ", ")
        if (xargs != "")
            args <- paste(args, xargs, sep = ", ")
        call <- paste("survey::svyglm(", args, ")", sep = "")
    } else if (design == "experiment") {
        # experimental design:
        stop("Experiments are not yet implemented. \n")
    }

    # at this stage we just return the call
    call
}


#' Fit a survey design
#'
#' Fit a survey design to an object
#' @param svydes a design
#' @param dataset.name a dataset name
#' @return a survey object
#' @author Tom Elliott
#' @export
fitDesign <- function(svydes, dataset.name) {
    if (all(svydes == "")) return()
    svy.des <- paste0(
        "survey::svydesign(",
        paste(svydes, collapse = ", "),
        ", data = ",
        dataset.name,
        ")"
    )

    eval(parse(text = svy.des), .GlobalEnv)
}
