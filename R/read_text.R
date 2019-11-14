#' Read text as data
#'
#' The text can also be the value `"clipboard"` which
#' will use `readr::clipboard()`.
#'
#' @param text character string
#' @return data.frame
#' @author Tom Elliott
#' @export
read_text <- function(txt, delim = "\t", ...) {
    if (txt == "clipboard")
        txt <- readr::clipboard()

    readr::read_delim(txt, delim = delim)
}
