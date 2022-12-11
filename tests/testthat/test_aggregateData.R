test_that("Aggregation over single variable works", {
    expect_silent(
        iris_agg <- aggregateData(iris, "Species", c("count", "mean"))
    )
    expect_equal(
        iris_agg,
        iris %>%
            dplyr::group_by(Species) %>%
            dplyr::summarize(
                count = dplyr::n(),
                Sepal.Length_mean = mean(Sepal.Length),
                Sepal.width_mean = mean(Sepal.Width),
                Petal.Length_mean = mean(Petal.Length),
                Petal.Width_mean = mean(Petal.Width),
                .groups = "drop"
            ),
        ignore_attr = TRUE
    )
    expect_equal(
        iris_agg,
        eval(parse(text = code(iris_agg))),
        ignore_attr = TRUE
    )

    # quantiles
    expect_silent(
        iris_agg <- aggregateData(iris, "Species", c("quantile"))
    )
    expect_equal(
        iris_agg$Sepal.Width_q25,
        tapply(iris$Sepal.Width, iris$Species, quantile, probs = 0.25),
        ignore_attr = TRUE
    )
    expect_equal(eval(parse(text = code(iris_agg))), iris_agg,
        ignore_attr = TRUE)
})

test_that("Aggregating over fewer than all cat vars is OK", {
    d <- smart_read('cas500.csv')
    expect_s3_class(
        aggregateData(d, c("travel", "gender"), c("sum")),
        "data.frame"
    )
})

test_that("Error if no variables to aggregate", {
    d <- smart_read('cas500.csv')
    d <- d[, c("gender", "travel", "getlunch")]
    expect_error(aggregateData(d, "gender", "sum"))
})

require(survey)
data(api)
svy <- svydesign(~dnum+snum, weights = ~pw, fpc = ~fpc1+fpc2, data = apiclus2)

test_that("Aggregating survey data is valid", {
    expect_silent(
        svy_agg <- aggregateData(svy, "stype", c("count", "mean"), c("api99", "api00"))
    )

    svy_tbl <- srvyr::as_survey_design(svy)
    svy_tbl_agg <- svy_tbl %>%
        dplyr::group_by(stype) %>%
        dplyr::summarize(
            count = srvyr::survey_total(),
            api99_mean = srvyr::survey_mean(api99),
            api00_mean = srvyr::survey_mean(api00)
        )

    expect_equal(svy_agg, svy_tbl_agg, ignore_attr = TRUE)
    # code(svy_agg)

    # IQR
    expect_silent(
        svy_agg <- aggregateData(svy, "stype", c("iqr"), c("api99", "api00"))
    )
    expect_equal(
        svy_agg,
        eval(parse(text = code(svy_agg))),
        ignore_attr = TRUE
    )

    # Quantiles
    expect_silent(
        svy_agg <- aggregateData(svy, "stype", "quantile", c("api99", "api00"))
    )
    # -- srvyr::survey_quantile passing methods for old svyquantile
    if (utils::packageVersion("srvyr") <= "1.0.1" && utils::packageVersion("survey") >= "4.1") {
        skip("Old version of srvyr")
    }

    skip_if(packageVersion("survey") < package_version('4.2'))
    expect_equal(
        svy_agg$api99_q25,
        svyby(~api99, ~stype, svy, survey::svyquantile,
            quantiles = 0.25, keep.var = FALSE, ci = FALSE)$statistic
    )
    expect_equal(eval(parse(text = code(svy_agg))), svy_agg, ignore_attr = TRUE)
})
