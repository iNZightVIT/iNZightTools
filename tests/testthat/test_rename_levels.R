cas <- smart_read("cas500.csv")

test_that("Ranking works for iid data", {
    d <- rename_levels(cas, "gender", list(m = "male", f = "female"))
    expect_equal(
        levels(d$gender.renamed),
        c("f", "m")
    )
    check_eval(d)
})

test_that("Spaces in new level names are OK", {
    expect_silent(
        d <- rename_levels(cas, "cellsource", list("pocket money" = "pocket", "parents" = "parent"))
    )
    expect_equal(
        levels(d$cellsource.renamed),
        c("job", "other", "parents", "pocket money")
    )
    check_eval(d)
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Rename works for surveys", {
    d <- rename_levels(
        svy, "stype",
        list(Elementary = "E", High = "H", Middle = "M")
    )
    expect_s3_class(d, "survey.design2")
    expect_equal(
        levels(d$variables$stype.renamed),
        c("Elementary", "High", "Middle")
    )
    check_eval(d)
})
