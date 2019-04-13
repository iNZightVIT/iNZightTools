context("Test data joins")

stripattr <- function(x, attrs = c('code', 'join_cols')) {
  for (attr in attrs) attributes(x)[[attr]] <- NULL
  x
}

d1 <- readr::read_csv("join.csv")
d2 <- readr::read_csv("join2.csv")
d3 <- readr::read_csv("join3.csv")

test_that("Auto detection works", {
    skip("Skipping auto detection ...")
    expect_equal(
        stripattr(joindata(d1, d2, "", "", "inner_join", "x", "y")),
        dplyr::inner_join(d1, d2)
    )
})

test_that("Inner join works", {
    expect_equal(
        stripattr(joindata(d1, d3,
            list("x1", "x2"), list("x1", "x6"),
            "inner_join", "x", "y"
        )),
        dplyr::inner_join(d1, d3, by = c("x1" = "x1", "x2" = "x6"))
    )
    expect_equal(
        stripattr(joindata(d1, d2, "x1", "x1", "inner_join", "x", "y")),
        dplyr::inner_join(d1, d2, by = c("x1" = "x1"))
    )
})

test_that("Left join works", {
    expect_equal(
        stripattr(joindata(d1, d3,
            list("x1", "x2"), list("x1", "x6"),
            "left_join", "x", "y"
        )),
        dplyr::left_join(d1, d3, by = c("x1" = "x1", "x2" = "x6"))
    )
    expect_equal(
        stripattr(joindata(d1, d2, "x1", "x1", "left_join", "x", "y")),
        dplyr::left_join(d1, d2, by = c("x1" = "x1"))
    )
})

test_that("Full join works", {
    expect_equal(
        stripattr(joindata(d1, d3,
            list("x1", "x2"), list("x1", "x6"),
            "full_join", "x", "y"
        )),
        dplyr::full_join(d1, d3, by = c("x1" = "x1", "x2" = "x6"))
    )
    expect_equal(
        stripattr(joindata(d1, d2, "x1", "x1", "full_join", "x", "y")),
        dplyr::full_join(d1, d2, by = c("x1" = "x1"))
    )
})

test_that("Semi join works", {
    expect_equal(
        stripattr(joindata(d1, d3,
            list("x1", "x2"), list("x1", "x6"),
            "semi_join", "x", "y"
        )),
        dplyr::semi_join(d1, d3)
    )
    expect_equal(
        stripattr(joindata(d1, d2, "x1", "x1", "semi_join", "x", "y")),
        dplyr::semi_join(d1, d2, by = c("x1" = "x1"))
    )
})

test_that("Anti join works", {
    expect_equal(
        stripattr(joindata(d1, d3,
            list("x1", "x2"), list("x1", "x6"),
            "anti_join", "x", "y"
        )),
        dplyr::anti_join(d1, d3)
    )
    expect_equal(
        stripattr(joindata(d1, d2, "x1", "x1", "anti_join", "x", "y")),
        dplyr::anti_join(d1, d2, by = c("x1" = "x1"))
    )
})

test_that("Inbalanced columns are returned with an error", {
    expect_error(
        stripattr(joindata(d1, d3,
            list("x1", "x2"), list("x1", ""),
            "anti_join", "x", "y"
        ))
    )
})


