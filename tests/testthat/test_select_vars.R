context("Select vars")

test_that("Correct vars are grabbed", {
    expect_equivalent(
        selectVars(iris, c("Sepal.Length", "Species", "Sepal.Width")),
        iris[, c("Sepal.Length", "Species", "Sepal.Width")]
    )
})

test_that("Code is correct", {
    expect_equal(
        code(selectVars(iris, c("Sepal.Length", "Species", "Sepal.Width"))),
        "iris %>% dplyr::select(Sepal.Length, Species, Sepal.Width)"
    )
})
