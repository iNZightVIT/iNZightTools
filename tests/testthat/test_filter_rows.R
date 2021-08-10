test_that("Filtering code returns same result", {
    dat_filtered <- filterRows(iris, c(10, 20, 30))
    expect_equal(
        dat_filtered,
        eval(parse(text = code(dat_filtered))),
        ignore_attr = TRUE
    )
    expect_equal(dat_filtered, iris[-c(10, 20, 30), ], ignore_attr = TRUE)
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Filtering surveys is valid", {
    expect_silent(
        svy_filtered <- filterRows(svy, c(10, 20, 30))
    )
    expect_equal(svy_filtered, svy[-c(10, 20, 30), ], ignore_attr = TRUE)
})
