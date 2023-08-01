stripattr <- function(x, attrs = c("code", "join_cols")) {
    for (attr in attrs) attributes(x)[[attr]] <- NULL
    x
}

d1 <- readr::read_csv("join.csv", show_col_types = FALSE)
d2 <- readr::read_csv("join2.csv", show_col_types = FALSE)
d3 <- readr::read_csv("join3.csv", show_col_types = FALSE)

test_that("Auto detection works", {
    iris10 <- iris[1:10, ]
    expect_equal(
        stripattr(suppressWarnings(join_data(iris10, iris10))),
        suppressMessages(dplyr::inner_join(iris10, iris10, multiple = "all"))
    )
    expect_warning(join_data(iris, iris))
})

test_that("Inner join works", {
    expect_equal(
        stripattr(join_data(
            d1, d3,
            c("x1", x2 = "x6"),
            "inner", ".x", ".y"
        )),
        dplyr::inner_join(d1, d3, by = c("x1" = "x1", "x2" = "x6"))
    )
    expect_equal(
        stripattr(join_data(d1, d2, "x1", "inner", ".x", ".y")),
        dplyr::inner_join(d1, d2, by = c("x1" = "x1"))
    )
})

test_that("Left join works", {
    expect_equal(
        stripattr(join_data(
            d1, d3,
            c("x1", x2 = "x6"),
            "left", ".x", ".y"
        )),
        dplyr::left_join(d1, d3, by = c("x1" = "x1", "x2" = "x6"))
    )
    expect_equal(
        stripattr(join_data(d1, d2, "x1", "left", ".x", ".y")),
        dplyr::left_join(d1, d2, by = c("x1" = "x1"))
    )
})

test_that("Full join works", {
    expect_equal(
        stripattr(join_data(
            d1, d3,
            c("x1", x2 = "x6"),
            "full", ".x", ".y"
        )),
        dplyr::full_join(d1, d3, by = c("x1" = "x1", "x2" = "x6"))
    )
    expect_equal(
        stripattr(join_data(d1, d2, "x1", "full", ".x", ".y")),
        dplyr::full_join(d1, d2, by = c("x1" = "x1"))
    )
})

test_that("Semi join works", {
    expect_equal(
        stripattr(join_data(
            d1, d3,
            c("x1", x2 = "x6"),
            "semi", ".x", ".y"
        )),
        dplyr::semi_join(d1, d3, by = "x1")
    )
    expect_equal(
        stripattr(join_data(d1, d2, "x1", "semi", ".x", ".y")),
        dplyr::semi_join(d1, d2, by = c("x1" = "x1"))
    )
})

test_that("Anti join works", {
    expect_equal(
        stripattr(join_data(
            d1, d3,
            c("x1", x2 = "x6"),
            "anti", ".x", ".y"
        )),
        dplyr::anti_join(d1, d3, by = "x1")
    )
    expect_equal(
        stripattr(join_data(d1, d2, "x1", "anti", ".x", ".y")),
        dplyr::anti_join(d1, d2, by = c("x1" = "x1"))
    )
})

test_that("Inbalanced columns are returned with an error", {
    expect_error(
        stripattr(join_data(
            d1, d3,
            c(x1, x2 = ""),
            "anti", ".x", ".y"
        ))
    )
})
