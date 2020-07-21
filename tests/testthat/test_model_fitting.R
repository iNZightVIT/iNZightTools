context("Fit Model function")

test_that("Linear model formula are generated correctly", {
    expect_equal(
        fitModel("response", "x1", "d"),
        "lm(response ~ x1, data = d)"
    )
})

test_that("Family and link arguments are included", {
    expect_equal(
        fitModel("response", "x1", "d", family = "binomial"),
        "glm(response ~ x1, data = d, family = binomial)"
    )
    expect_equal(
        fitModel("response", "x1", "d", family = "binomial", link = "probit"),
        "glm(response ~ x1, data = d, family = binomial(link = \"probit\"))"
    )
})

test_that("Cox PH models are generated correctly", {
    expect_equal(
        fitModel("response", "x1", "d", surv = "cox", surv_params = c("time", "event")),
        "survival::coxph(survival::Surv(time, event) ~ x1, data = d, model = TRUE)"
    )

    expect_equal(
        fitModel("response", "x1 + x2", "d", design = "survey", surv = "cox", surv_params = c("time", "event")),
        "survey::svycoxph(survival::Surv(time, event) ~ x1 + x2, design = svy.design)"
    )
})
