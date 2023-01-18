cas <- smart_read("cas500.csv")

test_that("Collapsing 'string' factors", {
    expect_s3_class(
        cas1 <- collapse_cat(cas, "travel", levels = c("bike", "bus"), "test"),
        "data.frame"
    )
    expect_equal(
        levels(cas1$travel.coll),
        c("test", "motor", "other", "train", "walk")
    )
})

cas1 <- convert_to_cat(cas, "year")
cas2 <- collapse_cat(cas1, "year.cat", levels = c("13", "12", "11"), "test")

test_that("Collapsing 'number' factors", {
    expect_s3_class(cas2, "data.frame")
    expect_true("year.cat.coll" %in% names(cas2))
    expect_equal(levels(cas2$year.cat.coll), c(4:10, "test"))
})

test_that("Levels with special characters", {
    expect_s3_class(
        cas3 <- collapse_cat(cas, "travel", c("bike", "bus"), "bike/bus"),
        "data.frame"
    )
    expect_true("bike/bus" %in% levels(cas3$travel.coll))
    check_eval(cas3)
})


library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Survey collapsing works", {
    expect_silent(d <- collapse_cat(svy, "stype", c("E", "H"), "E_H"))
    expect_s3_class(d, "survey.design2")
    expect_equal(
        d$variables$stype.coll,
        forcats::fct_collapse(d$variables$stype, E_H = c("E", "H"))
    )
    check_eval(d)
})

svy2 <- convert_to_cat(svy, "dnum")

test_that("Survey collapse 'number' factors", {
    expect_silent(d <- collapse_cat(svy2, "dnum.cat", as.character(15), "15"))
    expect_true("dnum.cat.coll" %in% names(d$variables))
    check_eval(d)
})
