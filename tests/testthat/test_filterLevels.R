context("Filter levels of categorical variables")

dat <- readr::read_csv("cas500.csv")

# 1 LEVEL
filtered.1LVL <- filterLevels(dat, "cellsource", c("job"))
# formatR::tidy_source(text = code(filtered.1LVL), width.cutoff = 50)

# 3 LEVELS
filtered.3LVL <- filterLevels(dat, "getlunch", c("home", "tuckshop", "friend"))
# formatR::tidy_source(text = code(filtered.3LVL), width.cutoff = 50)



test_that("Result is only those with the filtered variable remains", {
  expect_true(all(levels(filtered.1LVL$cellsource) == c("job")))
  expect_true(all(levels(filtered.3LVL$getlunch) == c("home", "tuckshop", "friend")))
})

test_that("Result is the other categorical variables remain", {
  expect_true(all(levels(filtered.1LVL$travel) == levels(dat$travel)))
  expect_true(all(levels(filtered.1LVL$getlunch) == levels(dat$getlunch)))
  expect_true(all(levels(filtered.1LVL$gender) == levels(dat$gender)))
  expect_true(all(levels(filtered.3LVL$cellsource) == levels(dat$cellsource)))
  expect_true(all(levels(filtered.3LVL$travel) == levels(dat$travel)))
  expect_true(all(levels(filtered.3LVL$gender) == levels(dat$gender)))

})
