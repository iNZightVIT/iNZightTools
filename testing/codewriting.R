code <- c(
    "data <-", 
    "data %>%", 
    "mutate(", 
    "x2 =", 
    "x,", 
    "z =",
    "x + 2",
    ")"
)

indents <- c(0, 1, 2, 3, 4, 3, 4, 2)

txtCodeString <- function(code, ind) {
    z <- list(text = code, indent = ind)
    class(z) <- "txtcodestring"
    z
}

print.txtcodestring <- function(x, ...) {
    # cat(x$indent, ": ", x$text, "\n", sep = "")
    cat(sep = "",
        x$indent, ": ",
        rep(" ", x$indent*4), x$text, "\n")

    if (!is.null(x$subcode)) {
        # cat("---\n")
        sapply(x$subcode, print)
        # cat("+++\n")
    }
}

codeList <- list()
for (i in seq_along(1:length(code))) {
    codeList[[i]] <- txtCodeString(code[i], indents[i])
}


final <- list()

# final <- sumfun(codeList)

makeCodeList <- function(cl) {
    this <- cl[[1]]
    i0 <- this$indent
    sibs <- which(sapply(cl, function(x) x$indent == i0))

    N <- length(cl)
    sibs <- c(sibs, N+1)
    final <- list()
    for (i in seq_along(1:(length(sibs)-1))) {
        final[i] <- cl[sibs[i]]
        if (sibs[i] < N) {
            wi <- sibs[i] : (sibs[i + 1] - 1)
            if (length(wi) > 1)
                final[[i]]$subcode <- makeCodeList(cl[ wi[-1] ])
        }
    }
    final
}


makeCodeList(codeList) 

codelength <- function(cs, collapse = FALSE) {
  # - calculate how long this string is
  # - by looping over its children and calcualting how long they are
  #  (using codelength())
  # - until is.null(cs$subcode)
}

cancollapse <- function(cs) {

}
