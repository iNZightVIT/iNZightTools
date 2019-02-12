context("Test data joins")

stripcode <- function(x) {
  attributes(x)$code <- NULL
  x
}

d1 <- read_csv('Join.csv')
d3 <- read_csv('Join2.csv')
d2 <- d3 %>% select(x1, x3)

test_that("Auto detection works", {
  expect_equal(stripcode(joindata(d1, d2)), inner_join(d1, d2))
})