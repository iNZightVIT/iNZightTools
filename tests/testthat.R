library(testthat)
library(iNZightTools)

if (!requireNamespace("curl")) {
    stop("CURL IS NOT INSTALLED?!")
}

test_check("iNZightTools")
