context("Form class intervals from numeric variable")

d <- data.frame(x = round(runif(100, 0, 100), 2))
d$x2 <- round(d$x)

test_that("Basic equal-width intervals work", {
    expect_is(form_class_intervals(d, "x", n_intervals = 5L), "data.frame")
})

test_that("Constant width intervals work", {
    expect_is(form_class_intervals(d, "x", method = "width", interval_width = 10), "data.frame")
    expect_is(form_class_intervals(d, "x", method = "width", interval_width = 10, range = c(10, 90)), "data.frame")
    expect_is(
        form_class_intervals(d, "x", method = "width", interval_width = 10,
            range = c(10, 90), format = "a-b"),
        "data.frame"
    )
})

test_that("Equal-count intervals work", {
    expect_is(form_class_intervals(d, "x", method = "count", n_intervals = 5L), "data.frame")
    expect_is(form_class_intervals(d, "x2", method = "count", n_intervals = 5L), "data.frame")
})

test_that("Manual break points work", {
    expect_is(form_class_intervals(d, "x", method = "manual", break_points = c(0, 30, 40, 80, 100)), "data.frame")
})
