context("Sort data by value of variable(s)")

cas <- smart_read("cas500.csv")
# cas <- smart_read("tests/testthat/cas500.csv")

test_that("Sort works for iid data", {
    d <- sortVars(cas, c("height", "gender"))
    expect_equal(
        d$height,
        sort(cas$height, na.last = TRUE)
    )
    expect_equivalent(
        eval(parse(text = attr(d, "code"))),
        d
    )
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Sort works for surveys", {
    d <- sortVars(svy, "api00")
    expect_is(d, "survey.design2")
    expect_equal(
        d$variables$api00,
        sort(svy$variables$api00, na.last = TRUE)
    )
    expect_equivalent(
        eval(parse(text = attr(d, "code"))),
        d
    )
})
