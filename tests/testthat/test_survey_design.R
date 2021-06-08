context("Survey designs")

require(survey)
data(api)

test_that("Survey design file parsed correctly", {
    svyfile <- tempfile("apistrat", fileext = ".svydesign")
    svytoml <-
'strata = "stype"
weights = "pw"
fpc = "fpc"
'
    writeLines(svytoml, svyfile)

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
    svytoml <-
'ids = "dnum + snum"
weights = "pw"
fpc = "fpc1 + fpc2"
'
    writeLines(svytoml, svyfile)

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
    svytoml <-
'clusters = "dnum + snum"
weights = "pw"
fpc = "fpc1 + fpc2"
'
    writeLines(svytoml, svyfile)
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
    svytoml <-
'repweights = "rakedw[1-9]"
weights = "rakedw0"
reptype = "other"
scale = 1
rscales = 1
'
    writeLines(svytoml, svyfile)

    dchis <- svrepdesign(
        weights = ~rakedw0,
        repweights = "rakedw[1-9]",
        scale = 1,
        rscales = 1,
        type = "other",
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

test_that("Poststratification", {
    svyfile <- tempfile("apistrat", fileext = ".svydesign")
    svyTOML <- 'strata = "stype"
weights = "pw"
fpc = "fpc"

[calibrate.stype]
E = 4421
H = 755
M = 1018

[calibrate."sch.wide"]
"No" = 1072
"Yes" = 5122
'
    writeLines(svyTOML, svyfile)

    s <- import_survey(svyfile, apistrat)
    expect_is(s, "inzsvyspec")
    expect_is(s$design, "survey.design")

    expect_output(print(s), "survey::calibrate")
})

test_that("Survey designs can be parsed as survey spec", {
    dclus2 <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)
    dsvy <- as_survey_spec(dclus2)
    expect_is(dsvy, "inzsvyspec")
    expect_equal(dsvy$design, dclus2)
    expect_equal(dsvy$data, apiclus2)
    expect_equal(
        dsvy$spec,
        list(
            ids = "dnum + snum",
            probs = NULL,
            strata = NULL,
            fpc = "fpc1 + fpc2",
            nest = NULL,
            weights = "pw"
        )
    )
})
