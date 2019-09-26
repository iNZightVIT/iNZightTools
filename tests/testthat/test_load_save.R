context("Load/save RDA files")

cas <- smart_read("cas500.csv")
not_a_df <- 1:10
rda <- "my_files.rda"
on.exit(unlink(rda))

save(cas, iris, not_a_df, file = rda)

test_that("Load returns list of data frames", {
    res <- load_rda(rda)
    expect_is(res, "list")
    expect_equal(names(res), c("cas", "iris"))
    expect_equal(res$iris, iris)
    expect_equal(res$cas, cas)
})
