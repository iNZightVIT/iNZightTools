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
      a.dt = format(data$a, "%Y M%m"),
      .after = "a"
    )
  )
  expect_equal(
    stripcode(extract_part(data, "a", "Day of the week (name)", "a.dt")),
    data %>% tibble::add_column(
      a.dt = format(data$a, "%A"),
      .after = "a"
    )
  )
  expect_equal(
    stripcode(extract_part(data, "a", "Date only", "a.dt")),
    data %>% tibble::add_column(
      a.dt = "2020-07-07",
      .after = "a"
    )
  )
  expect_equal(
    stripcode(extract_part(data, "a", "Hour", "a.dt")),
    data %>% tibble::add_column(
      a.dt = format(data$a, "%H"),
      .after = "a"
    )
  )
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

