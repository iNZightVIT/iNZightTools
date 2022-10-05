skip_if_not_installed("validate")

data("women")
v <- validate::validator(height / weight < 0.5, mean(height) >= 0)
cf <- validate::confront(women, v)

test_that("confrontation summary percentages are correct", {
  orig.summary <- validate::summary(cf)

  expect_equal(
    validation_summary(cf)[["Fails (%)"]],
    sprintf("%1.2f%%", orig.summary$fails / orig.summary$items * 100)
  )
})

test_that("confrontation details are correct", {
  results <- validation_details(cf, v, "V1", NA, women)
  results <- results[(length(results) - 2):length(results)]
  expect_equal(
    gsub("  +", " ", results),
    with(women[1:3, ], sprintf("%d %d %d %.7f", 1:3, height, weight, height / weight))
  )
})
