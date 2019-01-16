context("Date time conversion functions")

data <- data.frame(
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
})

test_that("Invalid formats are dealt with appropriately", {
  expect_equal(
    convert_to_datetime(data, "y", "year month", "y.dt"),
    cbind(data, y.dt = NA)
  )
})

# 
# convert_to_datetime(data, "y", "year month", "y.dt") == cbind(data, y.dt = as.POSIXct(NA))
# 
# x = convert_to_datetime(data, "y", "year month", "y.dt")
# x
# class(x$y.dt)
# cbind(data, y.dt = "<NA>")
# class(as.POSIXct(NA))
# 
# a = as.POSIXct(NA)
# b = convert_to_datetime(data, "y", "year month", "y.dt")
# b = b$y.dt
# b
# a
# class(b)
# class(a)
# a == b
# 

xxt <- tryCatch(
  lubridate::parse_date_time("this is not a date", "ymd"),
  warning = function(w) if (w$message != "All formats failed to parse. No formats foun.") warning(w$message) else return(NA)
)
# iNZightTools::convert_to_datetime(data, "y", "year month", "y.dt")$y.dt -> na2
# 
# 
# varx = ""
# for (num in 1:length("y")) {
#   name = "y"[num]
#   varx = paste(varx, data[[name]])
# }
# convname = "year month"
# order_split = strsplit(convname, " ")
# convert = ""
# for (i in order_split) {
#   convert = paste(convert, "%", substring(i, 1, 1), sep = "", collapse = "")
# }
# 
# convert
# 
# converted = lubridate::parse_date_time(varx, convert)
# 
# converted
# 
# namevector = "y.dt"
# 
# data[ , namevector] = converted
# 
# data
# 
# exp = tibble::add_column(data, converted)
# 
# exp
# 
# names(exp)[length(names(exp))] = "y.dt"
# 
# exp
