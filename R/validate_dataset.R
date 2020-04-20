#' Validation Confrontation Summary
#'
#' Generates a summary of a confrontation which gives basic information about
#' each validation rule tested.
#'
#' @param cf Confrontation object from \code{validate::confront()}
#'
#' @return A \code{data.frame} with number of tests performed, number of
#' passes, number of failures, and failure percentage for each validation rule.
#'
#' @author Daniel Barnett
#' @export
validation_summary <- function(cf) {
    orig.summary <- validate::summary(cf)

    new.summary <- data.frame(
        rule = orig.summary$expression,
        stringsAsFactors = TRUE
    )

    new.summary$total <- orig.summary$items
    new.summary$passes <- orig.summary$passes
    new.summary$fails <- orig.summary$fails

    new.summary$percentfailed <-
        sprintf("%.2f%%", new.summary$fails / new.summary$total * 100)

    colnames(new.summary) <- c("Rule", "Total", "Passes", "Fails", "Fails (%)")

    new.summary
}

#' Details of Validation Rule Results
#'
#' Generates the more detailed text required for the details section in
#' \code{iNZValidateWin}.
#'
#' @param cf Confrontation object from \code{validate::confront()}
#' @param v Validator that generated \code{cf}
#' @param var Rule name to give details about
#' @param id.var Variable name denoting a unique identifier
#'        for each observation
#' @param df The dataset that was confronted
#'
#' @return A character vector giving each line of the summary detail text
#'
#' @author Daniel Barnett
#'
#' @export
validation_details <-  function(cf, v, var, id.var, df) {
    i <- which(names(v) == var)

    which.vars <- validate::variables(v$rules[[i]])

    n.fails <- validate::summary(cf)$fails[[i]]

    details.output <- c(
        sprintf("Summary of Validation Rule"),
        sprintf("--------------------------"),
        sprintf("Rule: \n  %s", capture.output(print(v$rules[[i]]@expr)))
    )

    if (n.fails == 0) {
        if (var %in% names(validate::errors(cf))) {
            details.table <-
                sprintf("Error occurred: \n  %s", validate::errors(cf)[var])
            n.fails <- NA
        } else {
            details.table <- "No observations failed this rule."
        }
    } else {
        if (class(validate::values(cf)) == "list") {
            rule.names <- lapply(validate::values(cf), colnames)

            which.list <- Position(function(x) var %in% x, rule.names)

            values <- validate::values(cf)[[which.list]][, var]
        } else {
            values <- validate::values(cf)[, var]
        }
        which.failed <- which(!values)

        inspect.vars <- df[which.failed, which.vars, drop = FALSE]

        which.expr <- v$rules[[i]]@expr[[2]]

        if (length(which.expr) > 1) {
            inspect.vars$`LHS` <- eval(which.expr, envir = inspect.vars)
            colnames(inspect.vars)[ncol(inspect.vars)] <-
                capture.output(print(which.expr))
        }

        if (!is.na(id.var)) {
            inspect.vars <- cbind(
                df[which.failed, id.var, drop = FALSE],
                inspect.vars
            )
        }

        details.table <- capture.output(inspect.vars)
    }

    details.output <- c(details.output, sprintf("Fails: \n  %s", n.fails))

    if (var %in% names(validate::warnings(cf))) {
        details.output <- c(
            details.output,
            sprintf("Warnings: \n  %s", validate::warnings(cf))
        )
    }

    c(
        details.output,
        "--------------------------",
        details.table
    )
}
