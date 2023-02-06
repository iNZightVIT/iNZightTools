cas <- smart_read("cas500.csv")

test_that("Either united column NAs remain as NAs", {
    options(inzighttools.max_levels = 500)
    cas2 <- combine_vars(cas, c("getlunch", "cellsource"), sep = "_")
    expect_s3_class(cas2$getlunch_cellsource, "factor")
    check_eval(cas2)
    expect_true("home_(Missing)" %in% levels(cas2$getlunch_cellsource))
})

test_that("Limit size of variable combinations", {
    options(inzighttools.max_levels = 110)
    expect_error(combine_vars(iris, vars = names(iris)), "110")
    options(inzighttools.max_levels = 200)
    expect_error(combine_vars(iris, vars = names(iris)), "200")
    options(inzighttools.max_levels = 100)
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Uniting variables works with survey object", {
    options(inzighttools.max_levels = Inf)
    d <- combine_vars(svy, c("enroll", "awards"), sep = "&", "new", keep_na = FALSE)
    expect_s3_class(d, "survey.design2")
    expect_s3_class(d$variables$new, "factor")
    check_eval(d)
    expect_true(any(is.na(d$variables$new)))
    options(inzighttools.max_levels = 100)
})
