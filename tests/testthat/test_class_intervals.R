d <- tibble::tibble(x = round(runif(100, 0, 100), 2), x2 = round(x))

test_that("Basic equal-width intervals work", {
    expect_s3_class(form_class_intervals(d, "x", n_intervals = 5L), "data.frame")
    check_eval(form_class_intervals(d, "x", n_intervals = 5L))
})

test_that("Constant width intervals work", {
    expect_s3_class(form_class_intervals(d, "x", method = "width", interval_width = 10), "data.frame")
    expect_s3_class(form_class_intervals(d, "x", method = "width", interval_width = 10, range = c(10, 90)), "data.frame")
    expect_s3_class(
        form_class_intervals(d, "x",
            method = "width", interval_width = 10,
            range = c(10, 90), format = "a-b"
        ),
        "data.frame"
    )
})

test_that("Equal-count intervals work", {
    expect_s3_class(form_class_intervals(d, "x", method = "count", n_intervals = 5L), "data.frame")
    expect_s3_class(form_class_intervals(d, "x2", method = "count", n_intervals = 5L), "data.frame")
})

test_that("Manual break points work", {
    expect_s3_class(
        form_class_intervals(d, "x", method = "manual", break_points = c(0, 30, 40, 80, 100)),
        "data.frame"
    )
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Survey vars can form class intervals", {
    d <- form_class_intervals(svy, "api00", method = "width", interval_width = 100)
    expect_equal(length(levels(d$variables$api00.f)), 5L)
    check_eval(d)
})
