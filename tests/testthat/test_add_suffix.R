test_that("Suffixes are added correctly", {
    expect_equal(
        add_suffix(
            c("data", "data.filtered", "data.filtered29", "data.filtered2.reshaped"),
            "filtered"
        ),
        c("data.filtered", "data.filtered2", "data.filtered30", "data.filtered3.reshaped")
    )
})
