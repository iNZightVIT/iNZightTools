cas <- smart_read("cas500.csv")
# cas <- smart_read("tests/testthat/cas500.csv")

test_that("Rename variables works for iid data", {
    d <- renameVars(cas, c(gender = "sex"))
    expect_false("gender" %in% names(d))
    expect_equal(d$sex, cas$gender)
    expect_equal(
        eval(parse(text = attr(d, "code"))),
        d,
        ignore_attr = TRUE
    )
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Rename variaables works for surveys", {
    d <- renameVars(svy, list(stype = "school type"))
    expect_s3_class(d, "survey.design2")
    expect_false("stype" %in% names(d$variables))
    expect_equal(d$variables$school_type, svy$variables$stype)
    expect_equal(
        eval(parse(text = attr(d, "code"))),
        d,
        ignore_attr = TRUE
    )
})
