cas <- smart_read("cas500.csv")

test_that("Numeric variables converted to missing/not-missing", {
    d <- missing_to_cat(cas, "rightfoot")
    expect_equal(
        d$rightfoot.miss,
        factor(ifelse(is.na(d$rightfoot), "(Missing)", "(Observed)"))
    )
    check_eval(d)
})

test_that("Categorical variables converted to missing", {
    d <- missing_to_cat(cas, "cellsource")
    expect_equal(
        as.character(d$cellsource.miss),
        ifelse(is.na(d$cellsource), "(Missing)", as.character(d$cellsource))
    )
    check_eval(d)
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Conversion to categorical works for surveys", {
    d <- missing_to_cat(svy, c("api00", "stype"))
    expect_s3_class(d, "survey.design2")
    check_eval(d)
})
