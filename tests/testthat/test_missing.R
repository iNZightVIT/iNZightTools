cas <- smart_read("cas500.csv")
# cas <- smart_read("tests/testthat/cas500.csv")

test_that("Numeric variables converted to missing/not-missing", {
    d <- missingToCat(cas, "rightfoot")
    expect_equal(
        d$rightfoot_miss,
        factor(ifelse(is.na(d$rightfoot), "missing", "observed"))
    )
    expect_equal(
        eval(parse(text = attr(d, "code"))),
        d,
        ignore_attr = TRUE
    )
})

test_that("Categorical variables converted to missing", {
    d <- missingToCat(cas, "cellsource")
    expect_equal(
        as.character(d$cellsource_miss),
        ifelse(is.na(d$cellsource), "missing", as.character(d$cellsource))
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

test_that("Conversion to categorical works for surveys", {
    d <- missingToCat(svy, c("api00", "stype"))
    expect_s3_class(d, "survey.design2")
    expect_equal(
        eval(parse(text = attr(d, "code"))),
        d,
        ignore_attr = TRUE
    )
})
