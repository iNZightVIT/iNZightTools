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

test_that("Survey design file parsed correctly", {
    svyfile <- tempfile("apiclus2", fileext = ".svydesign")
    write.dcf(data.frame(ids = "dnum + snum", weights = "pw", fpc = "fpc1+fpc2"), svyfile)

    s <- import_survey(svyfile)
    expect_is(s, "inzsvyspec")
    expect_null(s$design)

    s2 <- import_survey(svyfile, apiclus2)
    expect_is(s2, "inzsvyspec")
    expect_is(s2$design, "survey.design")

    expect_equal(
        {data <- apiclus2; make_survey(data, s)},
        s2
    )

    expect_output(print(s), "empty")
    expect_output(print(s2), "2 - level Cluster Sampling design")

    svyfile <- tempfile("apiclus2", fileext = ".svydesign")
    write.dcf(data.frame(clusters = "dnum + snum", weights = "pw", fpc = "fpc1+fpc2"), svyfile)
    s3 <- import_survey(svyfile, apiclus2)
    expect_equal(s2, s3)
})

test_that("Replicate weight designs", {
    skip_if_offline()
    skip_on_cran()

    chis_url <- "https://github.com/iNZightVIT/iNZight/raw/dev/tests/testthat/chis.csv"
    skip_if_not(RCurl::url.exists(chis_url))

    data <- smart_read(chis_url)

    svyfile <- tempfile("chis", fileext = ".svydesign")
    write.dcf(
        data.frame(
            repweights = "rakedw[1-9]",
            weights = "rakedw0",
            type = "other",
            scale = 1,
            rscales = 1
        ),
        svyfile
    )

    dchis <- svrepdesign(
        weights = ~rakedw0,
        repweights = "rakedw[1-9]",
        type = "other",
        scale = 1,
        rscales = 1,
        data = data
    )

    s <- import_survey(svyfile, data)
    expect_is(s, "inzsvyspec")
    expect_is(s$design, "svyrep.design")
    expect_equivalent(
        make_survey(data, s)$design,
        dchis
    )

})
