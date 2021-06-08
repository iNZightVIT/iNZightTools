context("Filter numeric variables")

dat <- readr::read_csv("cas500.csv")

# FOR "<"
filtered.L <- filterNumeric(dat, "rightfoot", "<", 15)
# formatR::tidy_source(text = code(filtered.L), width.cutoff = 50)

# FOR "<="
filtered.LE <- filterNumeric(dat, "height", "<=", 120)
# formatR::tidy_source(text = code(filtered.LE), width.cutoff = 50)

# FOR ">"
filtered.G <- filterNumeric(dat, "age", ">", 17)
# formatR::tidy_source(text = code(filtered.G), width.cutoff = 50)

# FOR ">="
filtered.GE <- filterNumeric(dat, "year", ">=", 10)
# formatR::tidy_source(text = code(filtered.GE), width.cutoff = 50)

# FOR "=="
filtered.E <- filterNumeric(dat, "armspan", "==", 140)
# formatR::tidy_source(text = code(filtered.E), width.cutoff = 50)

# FOR "!="
filtered.NE <- filterNumeric(dat, "cellcost", "!=", 0)
# formatR::tidy_source(text = code(filtered.NE), width.cutoff = 50)


test_that("Result is that only those that meet the logical condition remain", {
    expect_true(all(filtered.L$rightfoot < 15))
    expect_true(all(filtered.LE$height <= 120))
    expect_true(all(filtered.G$age > 17))
    expect_true(all(filtered.GE$year >= 10))
    expect_true(all(filtered.E$armspan == 140))
    expect_true(all(filtered.NE$cellcost != 0))
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Filtering surveys is valid", {
    expect_silent(
        svy_filtered <- filterNumeric(svy, "api00", "<", 700)
    )
    svy_filtered_proper <- subset(svy, api00 < 700)

    expect_equal(
        svymean(~api00, svy_filtered),
        svymean(~api00, svy_filtered_proper)
    )

    expect_equivalent(
        svy_filtered,
        eval(parse(text = code(svy_filtered)))
    )
})

# test_that("Filtering by survey structure variables prohibited ... ?", {
#     expect_error(
#         filterNumeric(svy, "dnum", "<", 1000)
#     )

#     subset(svy, dnum < 1000)
# })
