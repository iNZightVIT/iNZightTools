# Loading these libraries here so they don't produce
# messages when loading during use
suppressPackageStartupMessages({
    library(purrr)
    library(tsibble)
})

x <- tibble(x = lubridate::now())

test_that("Component extraction works for all `comp`", {
    expect_silent(map(names(inz_dt_comp), function(comp) {
        extract_dt_comp(x, "x", comp)
    }))
    expect_type(inz_dt_comp, "list")
})

test_that("gtree conversion of `inz_dt_comp` works", {
    tree_list <- get_dt_comp_tree(inz_dt_comp)
    expect_type(tree_list, "list")
    expect_equal(pluck_depth(tree_list), 4L)
})
