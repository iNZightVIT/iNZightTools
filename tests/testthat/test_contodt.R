context("Date time conversion functions")

data <- data.frame(
  a = "7th of July 2020 06:15:30 pm",
  b = "19560612",
  c = "20191901",
  d = "18561263123",
  x = "2018-01-01 12:20", 
  y = "this is not a date", 
  z = "12pm on the 8th day of march, 2017"
)
test_that("Strings are converted to date time objects", {
  expect_equal(
    convert_to_datetime(data, "x", "year month day Hour Minute", "x.dt"),
    cbind(data, x.dt = lubridate::ymd_hms("2018-01-01 12:20:00"))
  )
  expect_equal(
    convert_to_datetime(data, "z", "Hour pm/am day month year", "z.dt"),
    cbind(data, z.dt = lubridate::parse_date_time("12pm on the 8th day of march, 2017", "%H%p%d%m%y"))
  )
  expect_equal(
    convert_to_datetime(data, "a", "day month year Hour Minute Second pm/am", "a.dt"),
    cbind(data, a.dt = lubridate::parse_date_time("7th of July 2020 06:15:30 pm", "%d%m%y%H%M%S%p"))
  )
  expect_equal(
    convert_to_datetime(data, "b", "year month day", "b.dt"),
    cbind(data, b.dt = lubridate::parse_date_time("19560612", "%y%m%d"))
  )
  expect_equal(
    convert_to_datetime(data, "d", "Unix timestamp (secs from 1970)", "d.dt"),
    cbind(data, d.dt = as.POSIXct(as.numeric("18561263123"), origin = "1970-01-01"))
  )
})

test_that("Invalid formats are dealt with appropriately", {
  expect_equal(
    convert_to_datetime(data, "y", "year month", "y.dt"),
    cbind(data, y.dt = NA)
  )
  expect_equal(
    convert_to_datetime(data, "c", "year month day", "c.dt"),
    cbind(data, c.dt = NA)
  )
})

