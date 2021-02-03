context("Standardise (scale) variables")

cas <- smart_read("cas500.csv")
# cas <- smart_read("tests/testthat/cas500.csv")

test_that("Simple standardization", {
    d <- standardizeVars(cas, c("height", "rightfoot"))
    expect_equal(
        d$height.std,
        scale(cas$height)[,1]
    )
    expect_equivalent(
        eval(parse(text = attr(d, "code"))),
        d
    )
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Standardization works for surveys", {
    d <- standardizeVars(svy, c("api99", "api00"))
    expect_is(d, "survey.design2")
    mu <- svymean(~api00, svy, na.rm = TRUE)
    sd <- sqrt(svyvar(~api00, svy, na.rm = TRUE))
    expect_equal(
        d$variables$api00.std,
        (d$variables$api00 - mu) / sd
    )
    expect_equivalent(
        eval(parse(text = attr(d, "code"))),
        d
    )
})
