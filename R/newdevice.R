##' Opens a new graphics device
##'
##' Depending on the system, difference devices are better.
##' The windows device works fine (for now), only attempt to speed up
##' any other devices that we're going to be using.
##' We speed them up by getting rid of bufferring.
##'
##' @title Open a New Graphics Device
##' @param width the width (in inches) of the new device
##' @param height the height (in inches) of the new device
##' @param ... additional arguments passed to the new device function
##'
##' @return NULL
##'
##' @author Tom Elliott
##'
##' @export
newdevice <- function(width = 7, height = 7, ...) {

    if (.Platform$OS.type == "windows") {
        ## Windows
        grDevices::dev.new(width = width, height = height, ...)
    } else if (Sys.info()["sysname"] == "Darwin") {
        ## Mac - prefer Acinonyx if installed
        if (requireNamespace("Acinonyx", quietly = TRUE)) {
            # Acinonyx uses pixels rather than inches, convert inches to
            # pixels to determine dims. Assume 90 dpi.
            width.in <- round(width * 90)
            height.in <- round(height * 90)
            Acinonyx::idev(width = width.in, height = height.in, ...)
        } else {
            grDevices::dev.new(width = width, height = height, type = "nbcairo", ...)
        }
    } else {
        ## Linux - prefer cairoDevice over default
        if (requireNamespace("cairoDevice", quietly = TRUE)) {
            cairoDevice::Cairo(width = width, height = height, ...)
        } else {
            grDevices::dev.new(width = width, height = height, ...)
        }
    }

    invisible(NULL)

}
