#' Survey Specification helper functions
#'
#' These functions are just wrappers around the functions in 'surveyspec' so existing code continues to work.
#'
#' @param ... arguments passed to 'surveyspec' function
#' @return see 'surveyspec::import_survey'
#' @describeIn surveyspec Import a survey object
#' @export
import_survey <- function(...) {
    if (!requireNamespace("surveyspec", quietly = TRUE)) {
        stop("Please install `surveyspec` with remotes::install_github('tmelliott/surveyspec')")
    }

    surveyspec::import_survey(...)
}

#' @return see 'surveyspec::make_survey'
#' @describeIn surveyspec Make a survey
#' @export
make_survey <- function(...) {
    if (!requireNamespace("surveyspec", quietly = TRUE)) {
        stop("Please install `surveyspec` with remotes::install_github('tmelliott/surveyspec')")
    }

    surveyspec::make_survey(...)
}

#' @return see 'surveyspec::as_survey'
#' @describeIn surveyspec as survey method
#' @export
as_survey <- function(...) {
    if (!requireNamespace("surveyspec", quietly = TRUE)) {
        stop("Please install `surveyspec` with remotes::install_github('tmelliott/surveyspec')")
    }

    surveyspec::as_survey(...)
}

#' @return see 'surveyspec::as_survey_spec'
#' @describeIn surveyspec as survey spec method
#' @export
as_survey_spec <- function(...) {
    if (!requireNamespace("surveyspec", quietly = TRUE)) {
        stop("Please install `surveyspec` with remotes::install_github('tmelliott/surveyspec')")
    }

    surveyspec::as_survey_spec(...)
}

#' @return see 'surveyspec::write_spec'
#' @describeIn surveyspec write a survey specification
#' @export
write_spec <- function(...) {
    if (!requireNamespace("surveyspec", quietly = TRUE)) {
        stop("Please install `surveyspec` with remotes::install_github('tmelliott/surveyspec')")
    }

    surveyspec::write_spec(...)
}
