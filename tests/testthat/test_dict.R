skip_if_not_installed("expss")

cas <- smart_read("cas500_coded.csv")

test_that("Dictionary read and parsed correctly", {
    # devtools::load_all()
    dict <- read_dictionary("casdict.csv",
        name = "variable",
        title = "friendly_name"
    )
    expect_s3_class(dict, "dictionary")
    expect_s3_class(dict[[1]], "dict_var")
    expect_equal(names(dict), c(names(cas), "other"))

    expect_error(read_dictionary("casdict.csv"))

    expect_s3_class(as_tibble(dict), "tbl_df")
})

test_that("Dictionary columns renamed correctly", {
    td <- tempfile("dict", fileext = ".csv")
    on.exit(unlink(td))
    readr::write_csv(
        data.frame(
            vname = c("var1", "var2"),
            vtitle = c("title one", "title two"),
            vdesc = paste("this is variable", c("one", "two")),
            vcodes = c("0 | 1", NA_character_),
            vvals = c("one | two", NA_character_),
            xother = c("another", "value"),
            vtype = c("factor", "number")
        ),
        file = td
    )

    dict <- read_dictionary(td,
        name = "vname",
        title = "vtitle",
        description = "vdesc",
        codes = "vcodes",
        values = "vvals",
        type = "vtype"
    )
    expect_equal(
        names(as_tibble(dict)),
        c("name", "type", "title", "description", "code", "value", "xother")
    )
    expect_equal(
        names(as_tibble(dict, include_other = FALSE)),
        c("name", "type", "title", "description", "code", "value")
    )
})

test_that("Dictionaries can be added to datasets", {
    # devtools::load_all()
    dict <- read_dictionary("casdict.csv",
        name = "variable",
        title = "friendly_name"
    )

    cas_dict <- cas %>% apply_dictionary(dict)

    expect_s3_class(cas_dict$rightfoot, "units")
    expect_s3_class(cas_dict$cellsource, "factor")
    expect_equal(levels(cas_dict$cellsource), c("job", "other", "parent", "pocket"))

    expect_s3_class(cas_dict[[1]], "labelled")
    expect_equal(expss::var_lab(cas_dict$cellsource), "Cellphone money source")
})

test_that("Printing dictionaries is fine", {
    dict <- read_dictionary("casdict.csv",
        name = "variable",
        title = "friendly_name"
    )

    expect_output(print(dict[[1]]))

    expect_output(print(dict))
    expect_s3_class(print(dict, kable = TRUE), "knitr_kable")
    expect_s3_class(print(dict, kable = TRUE, code_sep = " | "), "knitr_kable")

    expect_s3_class(as_tibble(dict, include_other = TRUE), "tbl_df")
})
