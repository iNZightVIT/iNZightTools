context("Helper functions")

test_that("Vartype is correct", {
    expect_equal(vartype(as.POSIXct("2018-01-01 12:00:00")), "dt")
    expect_equal(vartype(as.Date("2018-01-01")), "dt")
    expect_equal(vartype(readr::parse_time("12:00:00")), "dt")
})
