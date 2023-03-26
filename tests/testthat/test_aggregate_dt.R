x <- tibble::tibble(x = lubridate::today() + seq_len(365), y = seq_len(365))

test_that("Aggregation of date-time works", {
    expect_silent(aggregate_dt(x, "x", "week", NULL, list(
        y = c("mean", "sd", "missing", "var", "sum")
    )))
    check_eval(aggregate_dt(x, "x", "week", NULL, list(y = "mean")))
})
