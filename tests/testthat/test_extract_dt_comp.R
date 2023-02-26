x <- tibble::tibble(x = lubridate::now())

test_that("Component extraction works for all `comp`", {
    expect_silent(purrr::map(names(inz_dt_comp), function(comp) {
        extract_dt_comp(x, "x", comp)
    }))
    expect_type(inz_dt_comp, "list")
})
