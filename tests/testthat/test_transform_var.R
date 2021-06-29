test_that("Reciprocal and square transformations work", {
    recip <- transformVar(iris, "Sepal.Length", "reciprocal", name = "x")
    expect_equal(recip$x, 1 / iris$Sepal.Length)
    expect_equal(
        eval(parse(text = code(recip))),
        recip,
        ignore_attr = TRUE
    )

    square <- transformVar(iris, "Sepal.Length", "square", name = "x")
    expect_equal(square$x, iris$Sepal.Length^2)
    expect_equal(
        eval(parse(text = code(square))),
        square,
        ignore_attr = TRUE
    )
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Transformation of survey variables works", {
    d <- transformVar(svy, "api00", "log")
    expect_s3_class(d, "survey.design2")
    expect_equal(d$variables$log.api00, log(svy$variables$api00))
    expect_equal(eval(parse(text = code(d))), d, ignore_attr = TRUE)
})
