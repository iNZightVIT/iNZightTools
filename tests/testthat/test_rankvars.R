cas <- smart_read("cas500.csv")
# cas <- smart_read("tests/testthat/cas500.csv")

test_that("Ranking works for iid data", {
    d <- rankVars(cas, c("rightfoot", "age"))
    expect_equal(
        d$rightfoot.rank,
        dplyr::min_rank(d$rightfoot)
    )
    expect_equal(
        eval(parse(text = attr(d, "code"))),
        d,
        ignore_attr = TRUE
    )
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Ranking works for surveys", {
    d <- rankVars(svy, c("api00", "enroll"))
    expect_s3_class(d, "survey.design2")
    expect_equal(
        eval(parse(text = attr(d, "code"))),
        d,
        ignore_attr = TRUE
    )
})
