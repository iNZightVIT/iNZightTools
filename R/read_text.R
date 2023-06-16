#' Read text as data
#'
#' The text can also be the value `"clipboard"` which
#' will use `readr::clipboard()`.
#'
#' @param txt character string
#' @param delim the delimiter to use, passed to `readr::read_delim()`
#' @param ... additional arguments passed to `readr::read_delim()`
#' @return data.frame
#' @author Tom Elliott
#' @export
read_text <- function(txt, delim = "\t", ...) {
    if (txt == "clipboard") {
        txt <- readr::clipboard()
    }

    if (utils::packageVersion("readr") < numeric_version("2.0.0")) {
        d <- readr::read_delim(I(txt), delim = delim)
    } else {
        d <- readr::read_delim(I(txt), delim = delim, show_col_types = FALSE)
    }

    d <- d |> dplyr::mutate_if(is.character, as.factor)

    d <- convert_strings(d)
    d <- validate_names(d)
    attr(d, "code") <- NULL
    d
}
