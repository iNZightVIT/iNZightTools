cas <- smart_read("cas500.csv")

test_that("Ranking works for iid data", {
    d <- rank_vars(cas, c("rightfoot", "age"))
    expect_equal(
        d$rightfoot.min_rank,
        dplyr::min_rank(d$rightfoot)
    )
    check_eval(d)
})

library(survey)
data(api)
svy <- svydesign(~ dnum + snum, weights = ~pw, fpc = ~ fpc1 + fpc2, data = apiclus2)

test_that("Ranking works for surveys", {
    d <- rank_vars(svy, c("api00", "enroll"))
    expect_s3_class(d, "survey.design2")
    check_eval(d)
})
