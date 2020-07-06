context("Survey designs")

require(survey)
data(api)

test_that("Survey design file parsed correctly", {
    svyfile <- tempfile("apistrat", fileext = ".svydesign")
    write.dcf(data.frame(strata = "stype", weights = "pw", fpc = "fpc"), svyfile)

    s <- import_survey(svyfile)
    expect_is(s, "inzsvyspec")
    expect_null(s$design)

    s2 <- import_survey(svyfile, apistrat)
    expect_is(s2, "inzsvyspec")
    expect_is(s2$design, "survey.design")

    expect_equal(
        {data <- apistrat; make_survey(data, s)},
        s2
    )

    expect_output(print(s), "empty")
    expect_output(print(s2), "Stratified Independent Sampling design")
})
