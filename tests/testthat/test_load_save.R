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

test_that("Load has valid code", {
    expect_equal(code(load_rda(rda)), "load('my_files.rda')")
})

test_that("Save writes file with correct name", {
    on.exit(unlink("irisdata.rda"))
    x <- save_rda(iris, "irisdata.rda", "my_iris")
    expect_true(x)
    expect_equal(code(x), "save(my_iris, file = 'irisdata.rda')")

    load("irisdata.rda")
    expect_equal(my_iris, iris)
})
