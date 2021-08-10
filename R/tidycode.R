#' Tidy code with correct indents and limit the code to the specific width
#'
#' @title iNZight Tidy Code
#' @param x character string or file name of the file containing messy code
#' @param incl_library logical, if true, the output code will contain library name
#' @param width the width of a line
#' @param indent how many spaces for one indent
#' @param outfile the file name of the file containing formatted code
#' @return formatted code, optionally written to `outfile`
#' @author Tom Elliott, Lushi Cai
#' @export
tidy_all_code <- function(x, width = 80, indent = 4, outfile,
                          incl_library = TRUE) {
    if (length(x) == 1 && file.exists(x))
        x <- readLines(x)

    allcode <- getText(x, incl_library)
    strvector <- sapply(allcode, tidy_code, width = width, indent = indent)
    splist <- strsplit(unlist(strvector), "\n")
    splist <- unlist(splist, use.names = FALSE)
    splist <- splist[splist != ""]

    if (!missing(outfile)) {
        return(write(splist, outfile))
    }

    splist
}

### tidy a single piece of code
tidy_code <- function(codeline, width, indent) {
    commentlines <- grepl("^#", codeline)
    if (all(commentlines)) return(codeline)
    codeline <- gsub("|>", "|>\n", codeline, fixed = TRUE)
    codeline[!commentlines] <- gsub(",\ +", ",\n", codeline[!commentlines])
    cf <- tempfile(fileext = ".R")
    on.exit(unlink(cf))
    writeLines(codeline, cf)
    z <- capture.output(
        styler::style_file(cf, indent = indent, scope = "tokens")
    )
    rm(z)
    readLines(cf)
}


### import txt file and library names can display or not display
getText <- function(code, incl_library) {
    # code <- readLines(x)
    code1 <- code[trimws(code) != ""]
    origin <- code1
    if (incl_library) {
        code1 <- code1
    } else {
        code2 <- vector()
        for (i in (1:length(code1))) {
            withpn <- code1[i]
            package <- gregexpr("::", withpn)
            if (!(package %in% -1)) {
                space <- gregexpr(" ", withpn)

                allname <- vector()
                for (i in (1:length(package[[1]]))) {
                    allname <- c(
                        allname,
                        substr(withpn,
                            space[[1]][max(which(space[[1]] < package[[1]][i]))] + 1,
                            package[[1]][i] + 1
                        )
                    )
                }
                allname <- paste0(allname, collapse = "|")
                without <- gsub(allname, "", withpn)
            } else {
                without <- code1[i]
            }
            code2 <- c(code2, without)
        }
        code1 <- code2[trimws(code2) != ""]
    }

    code <- trimws(code1[trimws(code1) != ""])

    code[code != ""]
}
