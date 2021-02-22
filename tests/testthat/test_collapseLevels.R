context("Collapse levels")

cas <- smart_read("cas500.csv")
# cas <- smart_read("tests/testthat/cas500.csv")

test_that("Collapsing 'string' factors", {
    expect_is(
        cas1 <- collapseLevels(cas, "travel", levels = c("bike", "bus")),
        "data.frame"
    )
    expect_equal(
        levels(cas1$travel.coll),
        c("bike_bus", "motor", "other", "train", "walk")
    )
})

test_that("Collapsing 'number' factors", {
    cas1 <- convertToCat(cas, "year")
    expect_is(
        cas2 <- collapseLevels(cas1, "year.cat", levels = c("13", "12", "11")),
        "data.frame"
    )
    expect_true("year.cat.coll" %in% names(cas2))
    expect_equal(
        levels(cas2$year.cat.coll),
        c(4:10, "13_12_11")
    )
})

test_that("Levels with special characters", {
    expect_is(
        cas3 <- collapseLevels(cas, "travel", c("bike", "bus"), "bike/bus"),
        "data.frame"
    )
    expect_true("bike/bus" %in% levels(cas3$travel.coll))
    expect_equivalent(cas3, eval(parse(text = code(cas3))))
})


require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Survey collapsing works", {
    expect_silent(
        d <- collapseLevels(svy, "stype", c("E", "H"))
    )
    expect_is(d, "survey.design2")
    expect_equal(
        d$variables$stype.coll,
        forcats::fct_collapse(d$variables$stype, E_H = c("E", "H"))
    )
    expect_equivalent(eval(parse(text = code(d))), d)
})

test_that("Survey collapse 'number' factors", {
    svy2 <- convertToCat(svy, "dnum")
    expect_silent(d <- collapseLevels(svy2, "dnum.cat", as.character(c(15, 63, 83))))
    expect_true("dnum.cat.coll" %in% names(d$variables))
    expect_equivalent(eval(parse(text = code(d))), d)
})
