#' Opens a new graphics device
#'
#' Depending on the system, difference devices are better.
#' The windows device works fine (for now), only attempt to speed up
#' any other devices that we're going to be using.
#' We speed them up by getting rid of buffering.
#'

#'
#' @title Open a New Graphics Device
#' @param width the width (in inches) of the new device
#' @param height the height (in inches) of the new device
#' @param ... additional arguments passed to the new device function
#'
#' @return NULL
#' @author Tom Elliott
#' @export
newdevice <- function(width = 7, height = 7, ...) {
    # This function attempts to use \code{Acinonyx::idev} on macOS, and
    # \code{cairoDevice::Cairo} on Linux---however, these expressions
    # are quoted to prevent the package from requiring them in the
    # Suggests field, otherwise there are lots of problems
    # passing CRAN checks (mostly because of Acinonyx).
    if (.Platform$OS.type == "windows") {
        ## Windows
        grDevices::dev.new(width = width, height = height, ...)
    } else if (Sys.info()["sysname"] == "Darwin") {
        ## Mac - prefer Acinonyx if installed
        acinonyx.exists <-
            inherits(try(find.package("Acinonyx"), silent = TRUE), "try-error")
        if (acinonyx.exists) {
            # Acinonyx uses pixels rather than inches, convert inches to
            # pixels to determine dims. Assume 90 dpi.
            width.in <- round(width * 90)
            height.in <- round(height * 90)
            acinonyxDev <- eval(parse(text = "Acinonyx::idev"))
            acinonyxDev(width = width.in, height = height.in, ...)
        } else {
            grDevices::dev.new(width = width, height = height, ...)
        }
    } else {
        ## Linux - prefer cairoDevice over default
        cairo.exists <-
            inherits(try(find.package("cairoDevice"), silent = TRUE), "try-error")
        if (cairo.exists) {
            cairoDev <- eval(parse(text = "cairoDevice::Cairo"))
            cairoDev(width = width, height = height, ...)
        } else {
            grDevices::dev.new(width = width, height = height, ...)
        }
    }

    invisible(NULL)
}
