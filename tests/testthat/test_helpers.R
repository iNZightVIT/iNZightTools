context("Helper functions")

test_that("Vartype is correct", {
    expect_equal(vartype(as.POSIXct("2018-01-01 12:00:00")), "dt")
    expect_equal(vartype(as.Date("2018-01-01")), "dt")
    expect_equal(vartype(readr::parse_time("12:00:00")), "dt")
})

test_that("Survey objects identified correctly", {
    data(api, package = "survey")
    dclus2<-svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)
    dclus2rep <- suppressWarnings(as.svrepdesign(dclus2))

    data(scd, package = "survey")
    repweights <- 2 *
        cbind(
            c(1,0,1,0,1,0),
            c(1,0,0,1,0,1),
            c(0,1,1,0,0,1),
            c(0,1,0,1,1,0)
        )
    scdrep <- suppressWarnings(
        svrepdesign(data = scd, type = "BRR", repweights = repweights,
            combined.weights = FALSE)
    )

    expect_false(is_survey(apiclus2))
    expect_true(is_survey(dclus2))
    expect_true(is_survey(dclus2rep))
    expect_false(is_survey(scd))
    expect_true(is_survey(scdrep))

    expect_false(is_svydesign(apiclus2))
    expect_true(is_svydesign(dclus2))
    expect_false(is_svydesign(dclus2rep))
    expect_false(is_svydesign(scd))
    expect_false(is_svydesign(scdrep))

    expect_false(is_svyrep(apiclus2))
    expect_false(is_svyrep(dclus2))
    expect_true(is_svyrep(dclus2rep))
    expect_false(is_svyrep(scd))
    expect_true(is_svyrep(scdrep))
})

test_that("Make names are unique", {
    expect_equal(make_names("var"), "var")
    expect_equal(make_names("var", "var_z"), "var")
    expect_equal(make_names("var", "var"), "var1")
    expect_equal(make_names(c("var", "var"), "var"), c("var1", "var2"))
})
