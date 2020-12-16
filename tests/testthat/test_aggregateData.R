context("Data aggregation")

test_that("Aggregation over single variable works", {
    expect_silent(
        iris_agg <- aggregateData(iris, "Species", c("count", "mean"))
    )
    expect_equivalent(
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
            )
    )
    expect_equivalent(
        iris_agg,
        eval(parse(text = code(iris_agg)))
    )
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

    expect_equivalent(svy_agg, svy_tbl_agg)
    # code(svy_agg)

    # IQR
    expect_silent(
        svy_agg <- aggregateData(svy, "stype", c("iqr"), c("api99", "api00"))
    )
    # expect_equivalent(
    #     svy_agg$api99_iqr,
    #     apply(svyby(~api99, ~stype, svy, svyquantile, quantiles = c(0.25, 0.75)), 1, diff)
    # )
    expect_equivalent(
        svy_agg,
        eval(parse(text = code(svy_agg)))
    )
})
