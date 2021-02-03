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

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Survey designs work", {
  suppressWarnings(
    d <- separate(svy, "avg.ed", "ed.a", "ed.b", sep = ".", check = "Column")
  )
  expect_is(d, "survey.design2")
  expect_true(all(c("ed.a", "ed.b") %in% names(d$variables)))
  expect_false("avg.ed" %in% names(d$variables))
  expect_equivalent(
    suppressWarnings(eval(parse(text = code(d)))),
    d
  )

  expect_error(separate(svy, "avg.ed", "ed.a", "ed.b", sep = ".", check = "Row"))
})
