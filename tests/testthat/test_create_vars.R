cas <- smart_read("cas500.csv")

test_that("Create variables in a standard data frame", {
    cas2 <- create_vars(cas, "ratio", "height / rightfoot")
    expect_equal(cas2$ratio, with(cas, height / rightfoot))
    check_eval(cas2)
})

test_that("Variables can be deleted", {
    expect_silent(cas2 <- delete_vars(cas, c("height", "rightfoot")))
    expect_false(any(c("height", "rightfoot") %in% names(cas2)))
    check_eval(cas2)
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Create variables in a survey design", {
    d <- create_vars(svy, "apidiff", "api00 - api99")
    expect_equal(d$variables$apidiff, with(svy$variables, api00 - api99))
    check_eval(d)
})

test_that("Survey variables can be deleted", {
    expect_silent(d <- delete_vars(svy, c("api00", "enroll")))
    expect_false(any(c("api00", "enroll") %in% names(d)))
    expect_equal(eval(parse(text = code(d))), d, ignore_attr = TRUE)
})
