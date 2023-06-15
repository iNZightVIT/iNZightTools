cas <- smart_read("cas500.csv")

test_that("Reordering levels works", {
    expect_s3_class(
        cas1 <- reorder_levels(cas, "getlunch",
            new_levels =
                c("dairy", "friend", "home", "school", "tuckshop", "none")
        ),
        "data.frame"
    )
    expect_true("getlunch.reord" %in% names(cas1))
    expect_equal(
        levels(reorder_levels(cas, "getlunch", auto = "freq")$getlunch.reord),
        names(sort(table(cas$getlunch), decreasing = TRUE))
    )
})

test_that("Reordering respects name argument", {
    expect_s3_class(
        cas1 <- reorder_levels(cas, "getlunch",
            new_levels =
                c("dairy", "friend", "home", "school", "tuckshop", "none"),
            name = "getlunch.reordered"
        ),
        "data.frame"
    )
    expect_true("getlunch.reordered" %in% names(cas1))
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Reordering survey variables works", {
    d <- reorder_levels(svy, "stype", c("H", "M", "E"))
    expect_equal(levels(d$variables$stype.reord), c("H", "M", "E"))
    check_eval(d)
})
