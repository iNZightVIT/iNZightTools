cas <- smart_read("cas500.csv")

test_that("Sort works for iid data", {
    d <- sort_vars(cas, c("height", "gender"))
    expect_equal(d$height, sort(cas$height, na.last = TRUE))
    check_eval(d)
})

test_that("Check for `asc` length", {
    expect_error(sort_vars(cas, "gender", asc = c(TRUE, TRUE)))
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Sort works for surveys", {
    d <- sort_vars(svy, "api00")
    expect_s3_class(d, "survey.design2")
    expect_equal(d$variables$api00, sort(svy$variables$api00, na.last = TRUE))
    check_eval(d)
})
