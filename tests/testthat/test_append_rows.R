cas <- smart_read("cas500.csv")

test_that("Rows can be appended", {
    expect_silent(c2 <- append_rows(cas[1:400, ], cas[-(1:400), ]))
    expect_equal(c2, cas, ignore_attr = TRUE)
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Survey returns error", {
    expect_error(append_rows(svy, cas), "Cannot append rows to surveys")
})
