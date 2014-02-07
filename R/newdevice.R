newdevice <- function(width = 7, height = 7, ...) {
    ##### Due to problem with "fatal error" when closing the window,
    #     this currently does nothing.
    
    # The windows device works fine (for now), only attempt to speed up
    # any other devices that we're going to be using.
    # We speed them up by getting rid of bufferring.
    ## if ("Acinonyx" %in% rownames(installed.packages())) {
    ##     # Acinonyx uses pixels rather than inches, convert inches to
    ##     # pixels to determine dims. Assume 90 dpi.
    ##     width.in <- round(width * 90)
    ##     height.in <- round(height * 90)
    ##     Acinonyx::idev(width = width.in, height = height.in)
    ## } else {
        if (.Platform$OS.type != "windows" && Sys.info()["sysname"] != "Darwin")
            dev.new(width = width, height = height, type = "nbcairo", ...)
        else
            dev.new(width = width, height = height, ...)
    ## }
}
