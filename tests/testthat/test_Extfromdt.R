context("Extract from a date time variable")

data <- data.frame(
  a = lubridate::parse_date_time("7th of July 2020 06:15:30 pm", "%d%m%y%H%M%S%p"),
  b = lubridate::parse_date_time("19700613", "%y%m%d")
)
stripcode <- function(x) {
  attributes(x)$code <- NULL
  x
}
test_that("Desired parts are extracted", {
  expect_equal(
    stripcode(extract_part(data, "a", "Year Month", "a.dt")),
    data %>% tibble::add_column(
      a.dt = factor(format(data$a, "%YM%m")),
      .after = "a"
    )
  )
  expect_equal(
    stripcode(extract_part(data, "a", "Year Quarter", "a.dt")),
    data %>% tibble::add_column(
      a.dt = factor(format(zoo::as.yearqtr(data$a), '%YQ%q')),
      .after = "a"
    )
  )
  expect_equal(
    stripcode(extract_part(data, "a", "Date only", "a.dt")),
    data %>% tibble::add_column(
      a.dt = as.Date("2020-07-07"),
      .after = "a"
    )
  )
  expect_equal(
    stripcode(extract_part(data, "a", "Hour", "a.dt")),
    data %>% tibble::add_column(
      a.dt = as.numeric(format(data$a, "%H")),
      .after = "a"
    )
  )

  expect_true(is_dt(extract_part(data, "a", "Time only", "time")$time))
})

test_that("Invalid parts are returned with NA", {
  ## now this "works" because its a datetime with empty time (so, midnight)
  # expect_equal(
  #   stripcode(extract_part(data, "b", "Hour", "b.dt")),
  #   data %>% tibble::add_column(b.dt = NA, .after = "b")
  # )
  # expect_equal(
  #   stripcode(extract_part(data, "b", "Time only", "b.dt")),
  #   data %>% tibble::add_column(b.dt = NA, .after = "b")
  # )
})

months <- as.Date(paste("2019", 1:12, "01", sep = "-"))
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
  "Saturday", "Sunday")

test_that("Factor levels have correct order", {
  expect_equal(
    levels(extract_part(data, "a", "Month (full)", "month")$month),
    format(months, "%B")
  )
  expect_equal(
    levels(extract_part(data, "a", "Month (abbreviated)", "month")$month),
    format(months, "%b")
  )

  expect_equal(
    levels(extract_part(data, "a", "Day of the week (name)", "dow")$dow),
    weekdays
  )
  expect_equal(
    levels(extract_part(data, "a", "Day of the week (abbreviated)", "dow")$dow),
    substr(weekdays, 1, 3)
  )
})
