context("Fit Model function")

test_that("Linear model formula are generated correctly", {
    expect_equal(
        fitModel("response", "x1", "d"),
        "lm(response ~ x1, data = d)"
    )
})

test_that("Family and link arguments are included", {
    expect_equal(
        fitModel("response", "x1", "d", family = "binomial"),
        "glm(response ~ x1, data = d, family = binomial)"
    )
    expect_equal(
        fitModel("response", "x1", "d", family = "binomial", link = "probit"),
        "glm(response ~ x1, data = d, family = binomial(link = \"probit\"))"
    )
})
