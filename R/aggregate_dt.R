#' Aggregate datetimes
#'
#' @param .data dataframe or tibble to aggregate
#' @param method the type of aggregation
#' @param key the key column
#' @param name the name of the variable
#' @return a data frame/tibble
#' @author Yiwen He
#' @export
aggregatedt <- function(.data, method, key, name) {

    mc <- match.call()
    dataname <- mc$.data

    left <- paste0(".DATA$", "left")
    right <- paste0(".DATA$", "right")

    if (method == "Yearly") {
        exp <- ~.DATA %>% tibble::add_column(.NAME = .DATA$left)
    } else if (method == "Quarterly" & key == "W") {
        exp <- ~.DATA %>%
            tibble::add_column(
                .NAME = paste0(
                    .LEFT, "Q", (as.numeric(.RIGHT) - 1) %/% 13 + 1
                ),
                .after = ".AFTER"
            )
    } else if (method == "Quarterly" & key == "M") {
        exp <- ~.DATA %>%
            tibble::add_column(
                .NAME = paste0(
                    .LEFT, "Q", (as.numeric(.RIGHT) - 1) %/% 3 + 1
                ),
                .after = ".AFTER"
            )
    }

    ## Replacing variables
    exp <- replaceVars(exp,
        .NAME = name,
        .LEFT = left,
        .RIGHT = right,
        .DATA = dataname,
        .AFTER = "right"
    )

    interpolate(exp)
}
