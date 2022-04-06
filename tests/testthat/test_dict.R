cas <- smart_read("cas500_coded.csv")

test_that("Dictionary read and parsed correctly", {
    # devtools::load_all()
    dict <- read_dictionary("casdict.csv",
        name = "variable",
        title = "friendly_name"
    )
    expect_s3_class(dict[[1]], "dict_var")
    expect_equal(names(dict), names(cas))

    expect_error(read_dictionary("casdict.csv"))
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
