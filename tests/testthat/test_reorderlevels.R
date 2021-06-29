cas <- smart_read("cas500.csv")
# cas <- smart_read("tests/testthat/cas500.csv")

test_that("Reordering levels works", {
    expect_s3_class(
        cas1 <- reorderLevels(cas, "getlunch",
            new_levels =
                c("dairy", "friend", "home", "school", "tuckshop", "none")
        ),
        "data.frame"
    )
    expect_true("getlunch.reord" %in% names(cas1))

    expect_equal(
        levels(reorderLevels(cas, "getlunch", freq = TRUE)$getlunch.reord),
        names(sort(table(cas$getlunch), decreasing = TRUE))
    )
})

test_that("Reordering respects name argument", {
    expect_s3_class(
        cas1 <- reorderLevels(cas, "getlunch",
            new_levels =
                c("dairy", "friend", "home", "school", "tuckshop", "none"),
            name = "getlunch.reordered"
        ),
        "data.frame"
    )
    expect_true("getlunch.reordered" %in% names(cas1))
})

test_that("Reordering twice works", {
    cas1 <- reorderLevels(cas, "getlunch",
        new_levels =
            c("dairy", "friend", "home", "school", "tuckshop", "none")
    )
    expect_s3_class(
        cas2 <- reorderLevels(cas1, "getlunch",
            new_levels =
                c("home", "dairy", "friend", "school", "tuckshop", "none")
        ),
        "data.frame"
    )
    expect_equal(
        names(cas2),
        c(
            "cellsource", "rightfoot", "travel", "getlunch", "getlunch.reord",
            "getlunch.reord2", "height", "gender", "age", "year",
            "armspan", "cellcost"
        )
    )
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Reordering survey variables works", {
    d <- reorderLevels(svy, "stype", c("H", "M", "E"))
    expect_equal(levels(d$variables$stype.reord), c("H", "M", "E"))
    expect_equal(eval(parse(text = code(d))), d, ignore_attr = TRUE)
})
