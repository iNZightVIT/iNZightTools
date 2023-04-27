test_that("Test transformation", {
    trans <- transform_vars(iris, "Sepal.Length", "log10", name = "x")
    expect_equal(trans$x, log(iris$Sepal.Length, base = 10))
    check_eval(trans)
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Transformation of survey variables works", {
    d <- transform_vars(svy, "api00", "log")
    expect_s3_class(d, "survey.design2")
    expect_equal(d$variables$log.api00, log(svy$variables$api00))
    expect_equal(eval(parse(text = code(d))), d, ignore_attr = TRUE)
})
