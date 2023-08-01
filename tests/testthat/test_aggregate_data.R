test_that("Aggregation of non-survey works", {
    expect_silent(
        iris_agg <- aggregate_data(iris, "Species", c("count", "mean"))
    )
    expect_equal(
        iris_agg,
        iris %>%
            dplyr::group_by(Species) %>%
            dplyr::summarise(
                count = dplyr::n(),
                Sepal.Length_mean = mean(Sepal.Length),
                Sepal.width_mean = mean(Sepal.Width),
                Petal.Length_mean = mean(Petal.Length),
                Petal.Width_mean = mean(Petal.Width),
                .groups = "drop"
            ),
        ignore_attr = TRUE
    )

    # quantiles
    expect_silent(
        iris_agg <- aggregate_data(iris, "Species", c("quantile"))
    )
    expect_equal(
        iris_agg$Sepal.Width_q25,
        tapply(iris$Sepal.Width, iris$Species, quantile, probs = 0.25),
        ignore_attr = TRUE
    )

    expect_silent(
        aggregate_data(iris, "Species", "mean", names = c(mean = "{var}_mu"))
    )
    expect_silent(
        aggregate_data(iris, "Species", c("mean", "sd"), names = c(mean = "{var}_mu"))
    )

    # `summaries` as named list
    d <- aggregate_data(iris, "Species", list(
        Sepal.Length = c("mean", "sd"),
        Sepal.Width = c("mean", "sd"),
        Petal.Length = c("sd", "missing"),
        Petal.Width = c("var", "sum")
    ))
    expect_true(length(names(d)) == 9)
    expect_warning(
        aggregate_data(iris, "Species", list(Sepal.Length = "sd"), "Sepal.Length")
    )
    expect_error(aggregate_data(iris, "Species", c(Sepal.Length = "sd")))
    expect_error(aggregate_data(iris, "Species", list("sd")))
})

test_that("Aggregating over fewer than all cat vars is OK", {
    d <- smart_read("cas500.csv")
    expect_s3_class(
        aggregate_data(d, c("travel", "gender"), c("sum", "missing")),
        "data.frame"
    )
})

test_that("Error if no variables to aggregate", {
    d <- smart_read("cas500.csv")
    d <- d[, c("gender", "travel", "getlunch")]
    expect_error(aggregate_data(d, "gender", "sum"))
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Aggregating survey data is valid", {
    expect_silent(
        svy_agg <- aggregate_data(svy, "stype", c("count", "mean"), c("api99", "api00"))
    )

    svy_tbl <- srvyr::as_survey_design(svy)
    svy_tbl_agg <- svy_tbl %>%
        dplyr::group_by(stype) %>%
        dplyr::summarise(
            count = srvyr::survey_total(),
            api99_mean = srvyr::survey_mean(api99),
            api00_mean = srvyr::survey_mean(api00)
        )
    expect_equal(svy_agg, svy_tbl_agg, ignore_attr = TRUE)

    # IQR
    expect_silent(
        svy_agg <- aggregate_data(svy, "stype", c("iqr"), c("api99", "api00"))
    )

    # Quantiles
    expect_silent(
        svy_agg <- aggregate_data(svy, "stype", "quantile", c("api99", "api00"))
    )
})
