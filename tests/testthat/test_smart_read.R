test_that("smart_read can figure out the file type", {
    expect_equal(guess_type("txt"), "meta")
    expect_equal(guess_type("dta"), "stata")
    expect_equal(guess_type("xls"), "excel")
    expect_equal(guess_type("sas7bdat"), "sas")
})

test_that("files parsed correctly", {
    expect_s3_class(smart_read("meta.txt"), "data.frame")
    expect_s3_class(smart_read("meta.csv"), "data.frame")
    expect_s3_class(smart_read("appbset1.sav"), "data.frame")
    expect_s3_class(smart_read("c5hw1.dta"), "data.frame")
    expect_s3_class(smart_read("test.sas7bdat"), "data.frame")
    expect_s3_class(smart_read("cars.xpt"), "data.frame")
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

test_that("smart_read returns code with necessary conversions included", {
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
        "haven::read_sas(\"test.sas7bdat\") %>% dplyr::mutate_at(\"gender\", as.factor)"
    )
    expect_equal(
        code(smart_read("cars.xpt")),
        "haven::read_xpt(\"cars.xpt\") %>% dplyr::mutate_at(\"MAKE\", as.factor)"
    )
})

test_that("smart_read doesn't output col_types spec", {
    expect_silent(x <- smart_read("cas.txt"))
})

test_that("Column type overrides are respected", {
    expect_equal(as.character(sapply(smart_read("cas500.csv"), class)),
                 c("factor", "numeric", "factor", "factor", "numeric",
                   "factor", "numeric", "numeric", "numeric", "numeric"))
    cas.yearcat <- smart_read("cas500.csv",
        column_types = c(year = "c", age = "n", travel = "c")
    )
    expect_equal(
        as.character(sapply(cas.yearcat, class)),
        c("factor", "numeric", "factor", "factor", "numeric",
          "factor", "numeric", "factor", "numeric", "numeric")
    )
    expect_equal(
        levels(cas.yearcat$year),
        as.character(4:13)
    )

    # null should also work
    expect_s3_class(smart_read("cas500.csv", column_types = NULL), "data.frame")
})

test_that("SAS Import num to cat works", {
    expect_silent(
        d <- smart_read("test.sas7bdat",
            column_types = c(q1 = "c", q2 = "c"))
    )
    expect_s3_class(d$q1, "factor")
    expect_s3_class(d$q2, "factor")
})

test_that("smart_read can handle spaces and comment-characters", {
    expect_s3_class(smart_read("characters.csv"), "data.frame")
})

test_that("smart_read can handle datetimes", {
    dt <- smart_read("dt.csv")
    expect_s3_class(dt$x, "Date")
    expect_s3_class(dt$y, "hms")
    expect_s3_class(dt$z, "POSIXct")
})

test_that("conversion to character or factor works for datetimes", {
    expect_silent(
        dt <- smart_read("dt.csv", column_types = c(x = "c", y = "c", z = "c"))
    )
    expect_s3_class(dt$x, "Date")
    expect_s3_class(dt$y, "hms")
    expect_s3_class(dt$z, "POSIXct")

    expect_silent(dt <- smart_read("dt.csv", column_types = c(x = "f")))
    expect_s3_class(dt$x, "factor")
})

test_that("converting numeric with some string values to cat behaves appropriately", {
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp))
    readr::write_csv(
        data.frame(x = 1:100, y = c(sample(1:2, 99, T), "text")),
        tmp
    )
    expect_silent(d <- smart_read(tmp, column_types = c(y = "c")))
    expect_s3_class(d$y, "factor")
    expect_true(all(levels(d) %in% c("1", "2", "text")))
})

test_that("changing column types in delimited file", {
    expect_silent(
        dt <- smart_read("cas.txt", column_types = c(education = "c"))
    )
    expect_s3_class(dt$education, "factor")
})

test_that("Reading (excel) files converts strings to factor", {
    dt <- smart_read("cas500.xls", na = "NA")
    expect_s3_class(dt$travel, "factor")
    expect_s3_class(dt$gender, "factor")
})

test_that("Read excel returns list of sheets as attribute", {
    dt <- smart_read("cas500.xls", preview = TRUE, na = "NA")
    expect_equal(sheets(dt), "Census at School-500")
    expect_match(
        code(smart_read("cas500.xls", sheet = "Census at School-500", na = "NA")),
        "sheet = \"Census at School-500\""
    )
})

test_that("Reading RDS works", {
    t <- chartr("\\", "/", file.path(tempdir(), "iris_data.rds"))
    on.exit(unlink(t))

    saveRDS(iris, t)
    expect_equal(smart_read(t), iris, ignore_attr = TRUE)
    expect_match(code(smart_read(t)), sprintf("readRDS\\(\"%s\"\\)", t))
})

test_that("URLs are supported", {
    url <- "https://www.stat.auckland.ac.nz/~wild/data/test/CensusAtSchool-500.xls"
    skip_if( !RCurl::url.exists(url), "URL not available." )

    file <- try(url_to_temp(url), silent = TRUE)
    skip_if(inherits(file, "try-error"))

    expect_match(file, "CensusAtSchool\\.500.xls")
    expect_true(file.exists(file))
})

test_that("Special characters are correctly replaced", {
    fb <- readr::read_csv("fbcomments.csv", col_types = "cc")
    expect_equal(
        names(validate_names(fb)),
        c("Is_the_answer_correct", "What_was_the_answer_given")
    )

    expect_equal(
        names(smart_read("fbcomments.csv")),
        c("Is_the_answer_correct", "What_was_the_answer_given")
    )
})

test_that("Final dot is preserved if it is in the original name", {
    d <- data.frame(x = 1:10, y. = 1:10, "y?" = 1:10, check.names = FALSE)
    expect_equal(
        names(validate_names(d)),
        c("x", "y.", "y")
    )
})

test_that("Duplicates are numbered", {
    d <- data.frame(x. = 1:10, "y!" = 1:10, "y?" = 1:10, check.names = FALSE)
    expect_equal(
        names(validate_names(d)),
        c("x.", "y", "y.1")
    )
})

test_that("JSON supported", {
    t <- tempfile(fileext = ".json")
    on.exit(unlink(t))

    jsonlite::write_json(iris, t)
    expect_equal(smart_read(t), iris, ignore_attr = TRUE)

    write.csv(iris, t, row.names = FALSE, quote = FALSE)
    expect_error(smart_read(t), "Unable to read file")
})

test_that("Columns with first >1000 rows NA are read as character, converted correctly", {
    skip_if_offline()
    url <- "https://www.stat.auckland.ac.nz/~wild/data/FutureLearn/NHANES2009-2012.csv"
    skip_if_not(RCurl::url.exists(url))
    d <- try(smart_read(url), silent = TRUE)
    skip_if(inherits(d, "try-error"))

    expect_s3_class(d, "data.frame")
    expect_s3_class(d$Race3, "factor")
    expect_false(all(is.na(d$Race3)))
    expect_type(d$Testosterone, "double")
})

test_that("Variable names are quoted as necessary", {
    expect_equal(quote_varname("hello"), "hello")
    expect_equal(quote_varname("9hello"), "`9hello`")
    expect_equal(quote_varname("hello-world"), "`hello-world`")
    expect_equal(quote_varname("hello_world"), "hello_world")
    expect_equal(quote_varname("_hello"), "`_hello`")
})
