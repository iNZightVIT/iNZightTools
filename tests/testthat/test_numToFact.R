data <- data.frame(x = 1:5)
context("Convert numeric to factor")

data <- data %>% numToCat("x")
test_that("result is a dataframe with the correct dimensions", {
})

test_that("result has code attribute", {
    expect_false(is.null(code(data)))
    expect_is(code(data), "character")
    expect_equal(length(code(data)), 3)
    expect_match(code(data)[1], "## Convert x to categorical")
})