cas <- smart_read("cas500.csv")
# cas <- smart_read("tests/testthat/cas500.csv")

test_that("Ranking works for iid data", {
    d <- renameLevels(cas, "gender", list(m = "male", f = "female"))
    expect_equal(
        levels(d$gender.rename),
        c("f", "m")
    )
    expect_equal(
        eval(parse(text = attr(d, "code"))),
        d,
        ignore_attr = TRUE
    )
})

test_that("Spaces in new level names are OK", {
    expect_silent(
        d <- renameLevels(cas, "cellsource", list("pocket money" = "pocket", "parents" = "parent"))
    )
    expect_equal(
        levels(d$cellsource.rename),
        c("job", "other", "parents", "pocket money")
    )
    expect_equal(
        eval(parse(text = attr(d, "code"))),
        d,
        ignore_attr = TRUE
    )
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Rename works for surveys", {
    d <- renameLevels(svy, "stype",
        list(Elementary = "E", High = "H", Middle = "M"))
    expect_s3_class(d, "survey.design2")
    expect_equal(
        levels(d$variables$stype.rename),
        c("Elementary", "High", "Middle")
    )
    expect_equal(
        eval(parse(text = attr(d, "code"))),
        d,
        ignore_attr = TRUE
    )
})
