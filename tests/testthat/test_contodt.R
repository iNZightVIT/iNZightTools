data <- data.frame(
  a = "7th of July 2020 06:15:30 pm",
  b = "19560612",
  c = "20191901",
  d = "18561263123",
  x = "2018-01-01 12:20",
  y = "this is not a date",
  z = "12pm on the 8th day of march, 2017"
)
stripcode <- function(x) {
  attributes(x)$code <- NULL
  x
}
test_that("Strings are converted to date time objects", {
  expect_equal(
    stripcode(convert_to_datetime(data, "x", "year month day Hour Minute", "x.dt")),
    data %>% tibble::add_column(
      x.dt = lubridate::ymd_hms("2018-01-01 12:20:00"),
      .after = "x"
    )
  )
  expect_equal(
    stripcode(convert_to_datetime(data, "z", "Hour pm/am day month year", "z.dt")),
    data %>% tibble::add_column(
      z.dt = lubridate::parse_date_time("12pm on the 8th day of march, 2017", "%H%p%d%m%y"),
      .after = "z"
    )
  )
  expect_equal(
    stripcode(convert_to_datetime(data, "a", "day month year Hour Minute Second pm/am", "a.dt")),
    data %>% tibble::add_column(
      a.dt = lubridate::parse_date_time("7th of July 2020 06:15:30 pm", "%d%m%y%H%M%S%p"),
      .after = "a"
    )
  )
  expect_equal(
    stripcode(convert_to_datetime(data, "b", "year month day", "b.dt")),
    data %>% tibble::add_column(
      b.dt = lubridate::parse_date_time("19560612", "%y%m%d"),
      .after = "b"
    )
  )
  expect_equal(
    stripcode(convert_to_datetime(data, "d", "Unix timestamp (secs from 1970)", "d.dt")),
    data %>% tibble::add_column(
      d.dt = as.POSIXct(as.numeric("18561263123"), origin = "1970-01-01"),
      .after = "d"
    )
  )
})

test_that("Invalid formats are dealt with appropriately", {
  expect_warning(
    expect_equal(
      convert_to_datetime(data, "y", "year month", "y.dt"),
      data
    ),
    "Failed to parse"
  )
  expect_warning(
    expect_equal(
      convert_to_datetime(data, "c", "year month day", "c.dt"),
      data
    ),
    "Failed to parse"
  )
})
