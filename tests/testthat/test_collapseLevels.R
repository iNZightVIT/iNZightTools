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
