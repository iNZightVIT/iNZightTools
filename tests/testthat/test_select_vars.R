test_that("Correct vars are grabbed", {
    d <- select_vars(iris, c("Sepal.Length", "Species", "Sepal.Width"))
    expect_equal(names(d), c("Sepal.Length", "Species", "Sepal.Width"))
    check_eval(d)
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Surveys supported", {
    d <- select_vars(svy, c("api00", "stype"))
    expect_equal(names(d$variables), c("api00", "stype"))
    check_eval(d)
})
