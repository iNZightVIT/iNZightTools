cas <- smart_read("cas500.csv")

test_that("Dictionary read and parsed correctly", {
    dict <- read_dictionary("casdict.csv", id = "var")
    expect_true("id" %in% names(dict))

    expect_error(read_dictionary("casdict.csv"))
})
