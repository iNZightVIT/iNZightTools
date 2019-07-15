context("Categorical variable functions")

cas <- smart_read("cas500.csv")

test_that("Combine categorical variables returns a factor", {
    cas2 <- combineCatVars(cas, c("travel", "gender"))
    expect_is(cas2$travel.gender, "factor")
})
