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
    if (txt == "clipboard")
        txt <- readr::clipboard()

    readr::read_delim(txt, delim = delim) %>%
        dplyr::mutate_if(is.character, as.factor)
}
