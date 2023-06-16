data <- data.frame(
    a = "7th of July 2020 06:15:30 pm",
    b = "19560612",
    x = "2018-01-01 12:20:00",
    z = "12pm on the 8th day of march, 2017"
)

test_that("Strings are converted to date time objects", {
    expect_equal(
        convert_to_datetime(data, "x"),
        data %>% tibble::add_column(
            x.datetime = lubridate::ymd_hms("2018-01-01 12:20:00", tz = ""),
            .after = "x"
        ),
        ignore_attr = TRUE
    )
    expect_equal(
        convert_to_date(data, "x"),
        data %>% tibble::add_column(
            x.date = lubridate::ymd("20180101"),
            .after = "x"
        ),
        ignore_attr = TRUE
    )
    expect_equal(
        convert_to_datetime(data, "z", "Hour pm/am day month year", "z.dt", tz = "UTC"),
        data %>% tibble::add_column(
            z.dt = lubridate::parse_date_time("12pm on the 8th day of march, 2017", "hpdmy"),
            .after = "z"
        ),
        ignore_attr = TRUE
    )
    expect_equal(
        convert_to_datetime(data, "a", "dmyHMSp", "a.dt", tz = "UTC"),
        data %>% tibble::add_column(
            a.dt = lubridate::parse_date_time("7th of July 2020 06:15:30 pm", "dmyHMSp"),
            .after = "a"
        ),
        ignore_attr = TRUE
    )
    expect_equal(
        convert_to_date(data, "b", "year month day", "b.dt"),
        data %>% tibble::add_column(
            b.dt = lubridate::ymd("19560612"),
            .after = "b"
        ),
        ignore_attr = TRUE
    )
})
