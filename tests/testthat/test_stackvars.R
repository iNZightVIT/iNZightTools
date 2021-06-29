cas <- smart_read("cas500.csv")

test_that("Stack variables in a data frame", {
    cas2 <- stackVars(cas, c("height", "rightfoot", "armspan"), "what", "measurement")
    expect_equal(
        cas2,
        tidyr::gather(cas, key = "what", value = "measurement", height, rightfoot, armspan),
        ignore_attr = TRUE
    )
    expect_equal(eval(parse(text = code(cas2))), cas2, ignore_attr = TRUE)
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Create variables in a survey design", {
    expect_error(
        stackVars(svy, c("api00", "api99"), "year", "api"),
        "Cannot stack data in a survey design"
    )
})
