library(testthat)
library(iNZightTools)

print(installed.packages()[, 1:2])
if (!requireNamespace("curl")) {
    stop("CURL IS NOT INSTALLED?!")
}

test_check("iNZightTools")
