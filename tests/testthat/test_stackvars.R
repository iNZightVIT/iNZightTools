context("Stacking variables")

cas <- smart_read("cas500.csv")

test_that("Stack variables in a data frame", {
    cas2 <- stackVars(cas, c("height", "rightfoot", "armspan"), "what", "measurement")
    expect_equivalent(
        cas2,
        tidyr::gather(cas, key = "what", value = "measurement", height, rightfoot, armspan)
    )
    expect_equivalent(eval(parse(text = code(cas2))), cas2)
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
