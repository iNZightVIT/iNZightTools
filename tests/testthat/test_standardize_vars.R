cas <- smart_read("cas500.csv")

test_that("Simple standardization", {
    d <- standardize_vars(cas, c("height", "rightfoot"))
    expect_equal(d$height.std, scale(cas$height)[, 1])
    check_eval(d)
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Standardization works for surveys", {
    d <- standardize_vars(svy, c("api99", "api00"))
    expect_s3_class(d, "survey.design2")
    expect_equal(
        d$variables$api00.std,
        (d$variables$api00 - svymean(~api00, svy, na.rm = TRUE)) /
            sqrt(svyvar(~api00, svy, na.rm = TRUE))
    )
    check_eval(d)
})
