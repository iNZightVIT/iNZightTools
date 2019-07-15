# iNZightTools/tests/test_separate.R
context("Separate columns by a string")

stripattr <- function(x) {
  attributes(x)$code <- NULL
  x
}

data <- data.frame(
  "B" = c("hi", "hello", "bye"), 
  "A" = c("ada.sd\\d-sd^fdsf", "B_37K", "C"),
  "C" = c("2019M02", "2019M03", "2019M04")
)

test_that("Basic separate works", {
  expect_equal(
    stripattr(separate(data, "C", "year", "month", "M", "Column")),
    tidyr::separate(data, C, into = c("year", "month"), sep = "M")
  )
})


test_that("Special symbols are escaped", {
  expect_equal(
    stripattr(suppressWarnings(separate(data, "A", "left", "right", ".", "Column"))),
    suppressWarnings(tidyr::separate(data, A, into = c("left", "right"), sep = "\\."))
  )
  
  expect_equal(
    stripattr(suppressWarnings(separate(data, "A", "aaa", "bbb", "\\", "Column"))),
    suppressWarnings(tidyr::separate(data, A, into = c("aaa", "bbb"), sep = "\\\\", extra = "merge"))
  )
})