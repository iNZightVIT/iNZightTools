test_that("Filtering code returns same result", {
    dat_filtered <- remove_rows(iris, c(10, 20, 30))
    expect_equal(dat_filtered, iris[-c(10, 20, 30), ], ignore_attr = TRUE)
    check_eval(dat_filtered)
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Filtering surveys is valid", {
    expect_silent(svy_filtered <- remove_rows(svy, c(10, 20, 30)))
    expect_equal(svy_filtered, svy[-c(10, 20, 30), ], ignore_attr = TRUE)
})
