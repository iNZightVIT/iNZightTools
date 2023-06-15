text <- "x\ty\tz
1\ta\t34
2\tb\t28
3\ta\t18
4\ta\t29
"

test_that("Tab-delim text reads", {
    expect_equal(
        read_text(text),
        readr::read_tsv(I(text), show_col_types = FALSE) %>% dplyr::mutate(y = as.factor(y))
    )
})

text <- "var 1\tvar 2
1\t2
3\t4
5\t6
"
test_that("Spaces in names converted to underscores", {
    d <- read_text(text)
    expect_equal(colnames(d), c("var_1", "var_2"))
})
