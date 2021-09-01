cas <- smart_read("cas500.csv")
# cas <- smart_read("tests/testthat/cas500.csv")

test_that("Either united column NAs remain as NAs", {
    cas2 <- unite(cas, "new", c("getlunch", "cellsource"), sep = "_")
    expect_s3_class(cas2$new, "factor")
    expect_equal(eval(parse(text = code(cas2))), cas2, ignore_attr = TRUE)
    expect_true(
        "home_NA" %in% levels(cas2$new)
    )
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Uniting variables works with survey object", {
    d <- unite(svy, "new", c("enroll", "awards"), sep = "&")
    expect_s3_class(d, "survey.design2")
    expect_s3_class(d$variables$new, "factor")
    expect_equal(eval(parse(text = code(d))), d, ignore_attr = TRUE)
    expect_true(
        any(grepl("NA&", d$variables$new))
    )
})
