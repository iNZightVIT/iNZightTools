context("Importing data via smart_read")

test_that("smart_read can figure out the file type", {
    expect_equal(guess_type("txt"), "meta")
    expect_equal(guess_type("dta"), "stata")
    expect_equal(guess_type("xls"), "excel")
    expect_equal(guess_type("sas7bdat"), "sas")
})

test_that("files parsed correctly", {
    expect_is(smart_read("meta.txt"), "data.frame")
    expect_is(smart_read("meta.csv"), "data.frame")
    expect_is(smart_read("appbset1.sav"), "data.frame")
    expect_is(smart_read("c5hw1.dta"), "data.frame")
    expect_is(smart_read("test.sas7bdat"), "data.frame")
    expect_is(smart_read("cars.xpt"), "data.frame")
})

test_that("smart_read gets correct column types and dims", {
    data <- smart_read("cas500.csv")
    expect_equal(names(data), c('cellsource', 'rightfoot', 'travel', 'getlunch', 'height',
        'gender', 'age', 'year', 'armspan', 'cellcost'))
    expect_equal(nrow(data), 500L)
})

test_that("smart_read can handle various encoding", {
    data <- smart_read("enc-latin.csv", encoding = "ISO-8859-1")
    expect_identical(as.character(data$summer[2]), "\u00e9t\u00e9")

    data <- smart_read("enc-latin.csv")
    expect_s3_class(data, "data.frame")
})

test_that("smart_read returns code!!", {
    expect_equal(
        code(smart_read("appbset1.sav")),
        "haven::read_sav(\"appbset1.sav\")"
    )
    expect_equal(
        code(smart_read("c5hw1.dta")),
        "haven::read_dta(\"c5hw1.dta\")"
    )
    expect_equal(
        code(smart_read("test.sas7bdat")),
        "haven::read_sas(\"test.sas7bdat\")"
    )
    expect_equal(
        code(smart_read("cars.xpt")),
        "haven::read_xpt(\"cars.xpt\")"
    )
})

test_that("smart_read can take column types", {
    expect_equal(as.character(sapply(smart_read("cas500.csv"), class)),
                 c("factor", "numeric", "factor", "factor", "numeric",
                   "factor", "numeric", "numeric", "numeric", "numeric"))
    expect_equal(as.character(sapply(smart_read(
        "cas500.csv",
        column_types = c(year = "c")), class)),
        c("factor", "numeric", "factor", "factor", "numeric",
          "factor", "numeric", "factor", "numeric", "numeric"))

    expect_is(smart_read("cas500.csv", column_types = NULL), "data.frame")
})

test_that("smart_read can handle spaces and comment-characters", {
    expect_s3_class(smart_read("characters.csv"), "data.frame")
})

test_that("smart_read can handle datetimes", {
    dt <- smart_read("dt.csv")
    expect_is(dt$x, "Date")
    expect_is(dt$y, "hms")
    expect_is(dt$z, "POSIXct")
})
