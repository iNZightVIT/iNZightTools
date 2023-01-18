dat <- readr::read_csv("cas500.csv", show_col_types = FALSE)

# FOR "<"
filtered.L <- filter_num(dat, "rightfoot", "<", 15)
# formatR::tidy_source(text = code(filtered.L), width.cutoff = 50)

# FOR "<="
filtered.LE <- filter_num(dat, "height", "<=", 120)
# formatR::tidy_source(text = code(filtered.LE), width.cutoff = 50)

# FOR ">"
filtered.G <- filter_num(dat, "age", ">", 17)
# formatR::tidy_source(text = code(filtered.G), width.cutoff = 50)

# FOR ">="
filtered.GE <- filter_num(dat, "year", ">=", 10)
# formatR::tidy_source(text = code(filtered.GE), width.cutoff = 50)

# FOR "=="
filtered.E <- filter_num(dat, "armspan", "==", 140)
# formatR::tidy_source(text = code(filtered.E), width.cutoff = 50)

# FOR "!="
filtered.NE <- filter_num(dat, "cellcost", "!=", 0)
# formatR::tidy_source(text = code(filtered.NE), width.cutoff = 50)


test_that("Result is that only those that meet the logical condition remain", {
    expect_true(all(filtered.L$rightfoot < 15))
    expect_true(all(filtered.LE$height <= 120))
    expect_true(all(filtered.G$age > 17))
    expect_true(all(filtered.GE$year >= 10))
    expect_true(all(filtered.E$armspan == 140))
    expect_true(all(filtered.NE$cellcost != 0))
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Filtering surveys is valid", {
    expect_silent(
        svy_filtered <- filter_num(svy, "api00", "<", 700)
    )
    svy_filtered_proper <- subset(svy, api00 < 700)

    expect_equal(
        svymean(~api00, svy_filtered),
        svymean(~api00, svy_filtered_proper)
    )

    check_eval(svy_filtered)
})
