#' Convert to datetime
#'
#' @param .data dataframe
#' @param factorname name of the variable
#' @param convname format
#' @param newname name of the new column
#'
#' @return dataframe with datetime column
#' @export
#' @author Yiwen He
convert_to_datetime <- function(.data, factorname, convname, newname) {

    mc <- match.call()
    dataname <- mc$.data

    ## Subsetting dataset to get varx
    fact_name <- paste0(".DATA$'", factorname, "'", collapse = ", ")
    fact_name <- paste0("paste(", fact_name,  ")")

    ## Working out the convert format
    order_split <- strsplit(convname, " ")
    convert_string <- ""
    for (i in order_split) {
        convert_string <- paste(
            convert_string,
            "%",
            substring(i, 1, 1),
            sep = "",
            collapse = ""
        )
    }

    ## Actual function
    if (convert_string == "%U%t%(%f%1") {
        fact_name <- paste0("as.numeric(", fact_name, ")")
        exp <- ~.DATA %>%
            tibble::add_column(
                .NAME = as.POSIXct(.VARX, origin = "1970-01-01"),
                .after = ".AFTER"
            )
    } else {
        exp <- ~.DATA %>%
            tibble::add_column(
                .NAME = lubridate::parse_date_time(.VARX, convert),
                .after = ".AFTER"
            )
    }

    ## Replacing variables
    exp <- replaceVars(exp,
        .NAME = newname,
        .VARX = fact_name,
        .DATA = dataname,
        .VARNAME = factorname,
        .AFTER = factorname[length(factorname)]
    )

    tryCatch(
        interpolate(exp, convert = convert_string),
        warning = function(w) {
            if (w$message == "All formats failed to parse. No formats found." |
                substr(w$message, 1, 10) == "PCRE patte") {
                warning("Failed to parse")
                return(.data)
            } else {
                warning(w$message)
            }
        }
    )

}
