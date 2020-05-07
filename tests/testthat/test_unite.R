context("Unite columns")

cas <- smart_read("cas500.csv")

test_that("Either united column NAs remain as NAs", {
    cas2 <- unite(cas, "new", c("getlunch", "cellsource"), sep = "_")
    # expect_is(cas2$new, "factor")
})
