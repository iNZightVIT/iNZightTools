context("Rename factor levels")

cas <- smart_read("cas500.csv")
# cas <- smart_read("tests/testthat/cas500.csv")

test_that("Ranking works for iid data", {
    d <- renameLevels(cas, "gender", list(m = "male", f = "female"))
    expect_equal(
        levels(d$gender.rename),
        c("f", "m")
    )
    expect_equivalent(
        eval(parse(text = attr(d, "code"))),
        d
    )
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Rename works for surveys", {
    d <- renameLevels(svy, "stype",
        list(Elementary = "E", High = "H", Middle = "M"))
    expect_is(d, "survey.design2")
    expect_equal(
        levels(d$variables$stype.rename),
        c("Elementary", "High", "Middle")
    )
    expect_equivalent(
        eval(parse(text = attr(d, "code"))),
        d
    )
})
