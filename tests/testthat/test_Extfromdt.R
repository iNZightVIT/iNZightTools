context("Extract from a date time variable")

data <- data.frame(
  a = lubridate::parse_date_time("7th of July 2020 06:15:30 pm", "%d%m%y%H%M%S%p"),
  b = lubridate::parse_date_time("19700613", "%y%m%d")
)
test_that("Desired parts are extracted", {
  expect_equal(
    extract_part(data, "a", "Year month", "a.dt"),
    cbind(data, a.dt = paste(lubridate::year(data$a), " M", sprintf("%02d", lubridate::month(data$a)), sep = ""))
  )
  expect_equal(
    extract_part(data, "a", "Day of the week (name)", "a.dt"),
    cbind(data, a.dt = lubridate::wday(data$a, label = TRUE))
  )
  expect_equal(
    extract_part(data, "a", "Date only", "a.dt"),
    cbind(data, a.dt = as.Date(data$a))
  )
  x = as.character(chron::times(strftime(data$a, "%H:%M:%S", tz = "UTC")))
  expect_equal(
    extract_part(data, "a", "Hours (decimal)", "a.dt"),
    cbind(data, a.dt = sapply(strsplit(x, ":"), function(x) {
      x = as.numeric(x)
      x[1]+(x[2]+x[3]*60)/60
    })
  ))
})

test_that("Invalid parts are returned with NA", {
  expect_equal(
    extract_part(data, "b", "Hours (decimal)", "b.dt"),
    cbind(data, b.dt = NA)
  )
  expect_equal(
    extract_part(data, "b", "Time only", "b.dt"),
    cbind(data, b.dt = NA)
  )
})

