context("Unite columns")

cas <- smart_read("cas500.csv")

test_that("Either united column NAs remain as NAs", {
    cas2 <- unite(cas, "new", c("getlunch", "cellsource"), sep = "_")
    # expect_is(cas2$new, "factor")
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Uniting variables works with survey object", {
    d <- unite(svy, "new", c("enroll", "awards"), sep = "&")
    expect_is(d, "survey.design2")
    expect_equivalent(eval(parse(text = code(d))), d)
})
