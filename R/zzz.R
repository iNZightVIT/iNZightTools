.onLoad <- function(libname, pkgname) {
    # TODO: check what Suggested packages aren't installed and prompt to install them

    if (is.null(getOption("inzighttools.comment"))) {
        options(inzighttools.comment = "#")
    }
}
