cas <- smart_read("cas500.csv")

test_that("Dictionary read and parsed correctly", {
    dict <- read_dictionary("casdict.csv", id = "var")
    expect_true("id" %in% names(dict))

    expect_error(read_dictionary("casdict.csv"))
})

test_that("Dictionaries can be added to datasets", {
    dict <- read_dictionary("casdict.csv", id = "var")

    cas_dict <- cas %>% add_dictionary(dict)
    expect_false(has_dictionary(cas))
    expect_true(has_dictionary(cas_dict))
    expect_equal(get_dictionary(cas_dict), dict)
})
