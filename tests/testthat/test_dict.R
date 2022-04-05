cas <- smart_read("cas500.csv")

test_that("Dictionary read and parsed correctly", {
    devtools::load_all()
    dict <- read_dictionary("casdict.csv",
        name = "variable",
        title = "friendly_name"
    )
    expect_true("id" %in% names(dict))

    expect_error(read_dictionary("casdict.csv"))
})

test_that("Dictionaries can be added to datasets", {
    dict <- read_dictionary("casdict.csv", name = "variable", title = "friendly_name")

    cas_dict <- cas %>% apply_dictionary(dict)
    expect_s3_class(cas_dict$rightfoot, "units")
})

# cas2 <- apply_labels(cas,
#     travel = "Travel method"
# )
