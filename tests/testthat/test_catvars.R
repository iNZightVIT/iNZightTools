cas <- smart_read("cas500.csv")

test_that("Combine categorical variables returns a factor", {
    cas2 <- combineCatVars(cas, c("travel", "gender"))
    expect_s3_class(cas2$travel.gender, "factor")
})

test_that("NAs in either variable return an NA", {
    cas2 <- combineCatVars(cas, c("gender", "cellsource"))
    expect_equal(
        sum(is.na(cas2$gender.cellsource)),
        sum(is.na(cas2$gender) | is.na(cas2$cellsource))
    )
    cas2 <- combineCatVars(cas, c("getlunch", "cellsource"), keep_empty = TRUE)
    expect_equal(
        sum(is.na(cas2$getlunch.cellsource)),
        sum(is.na(cas2$getlunch) | is.na(cas2$cellsource))
    )
    expect_equal(
        table(cas2$getlunch.cellsource)[["school.other"]],
        0
    )
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Survey cat vars can be combined", {
    d <- combineCatVars(svy, c("both", "awards"))
    expect_equal(levels(d$variables$both.awards), c("No.No", "Yes.No", "Yes.Yes"))
    expect_equal(eval(parse(text = code(d))), d, ignore_attr = TRUE)
})
