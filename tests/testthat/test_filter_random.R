dat <- readr::read_csv("cas500.csv", show_col_types = FALSE)

test_that("Filtering code returns same result", {
    set.seed(10)
    dat_filtered <- filterRandom(iris, n = 5, sample_size = 3)
    set.seed(10)
    expect_equal(
        dat_filtered,
        eval(parse(text = code(dat_filtered))),
        ignore_attr = TRUE
    )
    expect_equal(unique(dat_filtered$Sample.Number), as.factor(1:5))
    expect_equal(nrow(dat_filtered), 15L)
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Filtering surveys is valid", {
    expect_error(
        svy_filtered <- filterRandom(svy, n = 5, sample_size = 20)
    )
})
