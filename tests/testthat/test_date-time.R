data <- data.frame(x = "2018-01-01 12:20:00")

test_that("Strings are converted to date time objects", {
    expect_equal(
        convert_to_datetime(data, "x", "x.dt"),
        data %>% tibble::add_column(
            x.dt = lubridate::ymd_hms("2018-01-01 12:20:00", tz = ""),
            .after = "x"
        ),
        ignore_attr = TRUE
    )
    expect_equal(
        convert_to_date(data, "x", "x.dt"),
        data %>% tibble::add_column(
            x.dt = lubridate::ymd("20180101"),
            .after = "x"
        ),
        ignore_attr = TRUE
    )
})
