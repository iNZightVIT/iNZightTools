x <- tibble::tibble(x = lubridate::now())

test_that("Component extraction works for all `comp`", {
    expect_silent(purrr::map(names(inz_dt_comp), function(comp) {
        extract_dt_comp(x, "x", comp)
    }))
    expect_type(inz_dt_comp, "list")
})

test_that("gtree conversion of `inz_dt_comp` works", {
    tree_list <- get_dt_comp_tree(inz_dt_comp)
    expect_type(tree_list, "list")
    expect_equal(purrr::pluck_depth(tree_list), 4L)
})
