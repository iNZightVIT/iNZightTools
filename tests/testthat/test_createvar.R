context("Create new variables")

cas <- smart_read("cas500.csv")

test_that("Create variables in a standard data frame", {
    cas2 <- createNewVar(cas, "ratio", "height / rightfoot")
    expect_equal(cas2$ratio, with(cas, height / rightfoot))
    expect_equivalent(eval(parse(text = code(cas2))), cas2)
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Create variables in a survey design", {
    d <- createNewVar(svy, "apidiff", "api00 - api99")
    expect_equal(d$variables$apidiff, with(svy$variables, api00 - api99))
    expect_equivalent(eval(parse(text = code(d))), d)
})
