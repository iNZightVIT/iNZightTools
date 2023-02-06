data <- data.frame(
    "B" = c("hi", "hello", "bye"),
    "A" = c("ada.sd\\d-sd^fdsf", "B_37K", "C"),
    "C" = c("2019M02", "2019M03", "2019M04")
)

test_that("Basic separate works", {
    expect_silent(separate_var(data, "C", "year", "month", "M"))
    expect_silent(separate_var(data, "C", "year", "month", 4, method = "position"))
    expect_silent(separate_var(data, "C", by = "M", into = "rows"))
    expect_silent(separate_var(data, "C", by = 4, into = "rows", method = "position"))
})


library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Survey designs work", {
    svy$variables$avg.ed <- format(svy$variables$avg.ed)
    suppressWarnings(
        d <- separate_var(svy, "avg.ed", "ed.a", "ed.b", by = ".")
    )
    expect_s3_class(d, "survey.design2")
    expect_true(all(c("ed.a", "ed.b") %in% names(d$variables)))
    expect_false("avg.ed" %in% names(d$variables))
    check_eval(d)
    expect_error(separate_var(svy, into = "rows"))
})
