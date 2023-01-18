cas <- smart_read("cas500.csv")

test_that("Either united column NAs remain as NAs", {
    cas2 <- combine_vars(cas, c("getlunch", "cellsource"), sep = "_", "new")
    expect_s3_class(cas2$new, "factor")
    check_eval(cas2)
    expect_true("home_(Missing)" %in% levels(cas2$new))
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Uniting variables works with survey object", {
    d <- combine_vars(svy, c("enroll", "awards"), sep = "&", "new")
    expect_s3_class(d, "survey.design2")
    expect_s3_class(d$variables$new, "factor")
    check_eval(d)
    expect_true(any(stringr::str_detect(d$variables$new, "\\(Missing\\)")))
})
