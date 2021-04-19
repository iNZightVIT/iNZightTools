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

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Surveys supported", {
    d <- selectVars(svy, c("api00", "stype"))
    expect_equal(names(d$variables), c("api00", "stype"))
    expect_equivalent(eval(parse(text = code(d))), d)
})
