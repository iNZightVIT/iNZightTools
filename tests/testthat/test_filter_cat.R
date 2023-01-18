dat <- readr::read_csv("cas500.csv", show_col_types = FALSE)

filtered.1LVL <- filter_cat(dat, "cellsource", c("job"))
filtered.3LVL <- filter_cat(dat, "getlunch", c("home", "tuckshop", "friend"))

test_that("Result is only those with the filtered variable remains", {
    expect_true(all(levels(filtered.1LVL$cellsource) == c("job")))
    expect_true(all(levels(filtered.3LVL$getlunch) == c("home", "tuckshop", "friend")))
})

test_that("Result is the other categorical variables remain", {
    expect_true(all(levels(filtered.1LVL$travel) == levels(dat$travel)))
    expect_true(all(levels(filtered.1LVL$getlunch) == levels(dat$getlunch)))
    expect_true(all(levels(filtered.1LVL$gender) == levels(dat$gender)))
    expect_true(all(levels(filtered.3LVL$cellsource) == levels(dat$cellsource)))
    expect_true(all(levels(filtered.3LVL$travel) == levels(dat$travel)))
    expect_true(all(levels(filtered.3LVL$gender) == levels(dat$gender)))
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Filtering survey design works", {
    expect_silent(
        svy_filtered <- filter_cat(svy, "stype", "E")
    )
    svy_filtered_proper <- subset(svy, stype == "E")

    expect_equal(
        svymean(~api00, svy_filtered),
        svymean(~api00, svy_filtered_proper)
    )
    check_eval(svy_filtered)
})
