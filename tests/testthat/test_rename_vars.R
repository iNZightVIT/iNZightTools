cas <- smart_read("cas500.csv")

test_that("Rename variables works for iid data", {
    d <- rename_vars(cas, list(sex = "gender"))
    expect_false("gender" %in% names(d))
    expect_equal(d$sex, cas$gender)
    check_eval(d)
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Rename variaables works for surveys", {
    d <- rename_vars(svy, list(school_type = "stype"))
    expect_s3_class(d, "survey.design2")
    expect_false("stype" %in% names(d$variables))
    expect_equal(d$variables$school_type, svy$variables$stype)
    check_eval(d)
})
