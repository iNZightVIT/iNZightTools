context("Test data joins")

stripattr <- function(x, attrs = c('code', 'join_cols')) {
  for (attr in attrs) attributes(x)[[attr]] <- NULL
  x
}

data = readr::read_csv("~/iNZight/scripts/parisjoin.csv") #original
data2 = readr::read_csv("~/iNZight/scripts/parisjoin2.csv") #has the same column "Instagram photo" with completely different values
data3 = readr::read_csv("~/iNZight/scripts/parisjoin3.csv") #contains a column named "sdasdsds" which has the same value as "Instagram user"
data4 = readr::read_csv("~/iNZight/scripts/parisjoin4.csv") #contains the same column "Instagram photo" with one value that is the same to the original one

test_that("Auto detection works", {
  expect_equal(stripattr(joindata(d1, d2)), inner_join(d1, d2))
})

# 

