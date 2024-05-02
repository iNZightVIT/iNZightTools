library(testthat)
library(iNZightTools)

print(installed.packages()[, "Version"])
if (!requireNamespace("curl")) {
    stop("CURL IS NOT INSTALLED?!")
}

test_check("iNZightTools")
