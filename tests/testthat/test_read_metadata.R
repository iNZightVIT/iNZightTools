options(inzighttools.comment = "#")

test_that("Metadata is read correctly", {
    data <- smart_read("meta.csv")
    expect_s3_class(data, "data.frame")
    expect_equal(
        names(data),
        c(
            "cellsource", "rightfoot", "travel", "getlunch", "height",
            "gender", "age", "year", "armspan", "cellcost", "school", "phonebill"
        )
    )
    expect_equal(
        as.character(sapply(data, class)),
        c(
            "factor", "numeric", "factor", "factor", "numeric",
            "factor", "numeric", "numeric", "numeric", "numeric", "factor", "numeric"
        )
    )
})

test_that("Name is read", {
    expect_equal(attr(smart_read("meta.csv"), "name"), "Census at School (subset)")
    expect_equal(attr(smart_read("meta.txt"), "name"), "meta")
})

test_that("Factor level orders are respected", {
    data <- smart_read("meta.csv")
    # data <- smart_read('tests/testthat/meta.csv')
    expect_equal(levels(data$gender), c("male", "female"))
})

test_that("First row is read if no title/description given", {
    expect_equal(length(readMetaComments("meta.txt")$columns), 2)
    expect_equal(levels(smart_read("meta.txt")$gender), c("male", "female"))
})

test_that("Special characters are removed from factor levels", {
    expect_equal(
        levels(smart_read("meta_chars.csv")$var2),
        c("maori", "english", "francais")
    )
})

test_that("Multiple response data read correctly", {
    m <- smart_read("meta_multi.csv")
    expect_equal(
        names(m),
        c(
            "age", "tech_phone", "tech_pc", "tech_tablet", "tech_watch",
            "lunch_home", "lunch_shop", "lunch_school", "lunch_none"
        )
    )
    expect_equal(m$tech_phone, c(1L, 1L, 1L, 0L))
})

test_that("NA codes converted to NA", {
    m <- smart_read("meta_nacodes.csv")

    expect_equal(m$b, c(4, NA, 6, NA))
    expect_equal(m$c, c(10, 12, NA, NA))
    expect_equal(
        m$c_missing,
        factor(c("observed", "observed", "Refused", "Dont_Know"))
    )
})

test_that("Non-existent variables produce error", {
    expect_error(
        smart_read("meta_bad.csv"),
        "Some variables defined in metadata not in dataset"
    )
})
