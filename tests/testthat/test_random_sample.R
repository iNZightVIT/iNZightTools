dat <- readr::read_csv("cas500.csv", show_col_types = FALSE)

test_that("Filtering code returns same result", {
    set.seed(10)
    dat_filtered <- random_sample(iris, n = 5, sample_size = 3)
    set.seed(10)
    check_eval(dat_filtered)
    expect_equal(unique(dat_filtered$.group), as.factor(1:5))
    expect_equal(nrow(dat_filtered), 15L)
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Filtering surveys is valid", {
    expect_error(svy_filtered <- random_sample(svy, n = 5, sample_size = 20))
})
