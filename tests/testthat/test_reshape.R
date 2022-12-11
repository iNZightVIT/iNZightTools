dt1 <- data.frame(
    "Country" = c("A", "B", "C"),
    "v1999" = c("0.7K", "37K", "212K"),
    "v2000" = c("2K", "80K", "213K")
)
dt2 <- data.frame(
    "Country" = c(
        "Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan",
        "Brazil", "Brazil", "Brazil", "Brazil", "China", "China", "China", "China"
    ),
    "Year" = c(1999, 1999, 2000, 2000, 1999, 1999, 2000, 2000, 1999, 1999, 2000, 2000),
    "Type" = c(
        "cases", "population", "cases", "population", "cases", "population",
        "cases", "population", "cases", "population", "cases", "population"
    ),
    "Count" = c(
        745, 19987071, 2666, 20595360, 37737, 172006362, 80488, 174504898,
        212258, 1272915272, 213766, 1280428583
    )
)

stripattr <- function(x, attr = 'code') {
    attributes(x)[[attr]] <- NULL
    x
}

test_that("Reshape wide to long works", {
    expect_equal(
        suppressWarnings(
            stripattr(reshape_data(dt1, "", "", list("v1999", "v2000"), "Year", "Count", "wide"))
        ),
        suppressWarnings(
            tidyr::gather(dt1, key = "Year", value = "Count", c("v1999", "v2000")) %>%
                dplyr::mutate(Year = as.factor(Year), Count = as.factor(Count))
        )
    )
    expect_equal(
        stripattr(reshape_data(dt2, "", "", "Count", "xx", "yy", "wide")),
        tidyr::gather(dt2, key = "xx", value = "yy", "Count") %>%
            dplyr::mutate(xx = as.factor(xx))
    )
})

test_that("Reshape long to wide works", {
    expect_equal(
        stripattr(reshape_data(dt2, "Type", "Count", "", "", "", "long")),
        tidyr::spread(dt2, key = "Type", value = "Count")
    )
})

test_that("Reshape results are factors", {
    expect_s3_class(
        suppressWarnings(
            reshape_data(dt1, "", "", c("v1999", "v2000"), "Year", "Count", "wide")$Year
        ),
        "factor"
    )
})
