context("Read text from clipboard")

text <- "x\ty\tz
1\ta\t34
2\tb\t28
3\ta\t18
4\ta\t29
"

test_that("Tab-delim text reads", {
    expect_equal(
        read_text(text),
        readr::read_tsv(text)
    )
})

