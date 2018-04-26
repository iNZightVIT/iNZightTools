context("iNZread")

test_that("iNZread gets correct column types and dims", {
    data <- iNZread("cas500.csv")
    expect_equal(names(data), c('cellsource', 'rightfoot', 'travel', 'getlunch', 'height',
        'gender', 'age', 'year', 'armspan', 'cellcost'))
    expect_equal(nrow(data), 500L)
})

test_that("iNZread can handle various encoding", {
    data <- iNZread("enc-latin.csv", encoding.style = "ISO-8859-1")
    expect_identical(data$summer[2], "\u00e9t\u00e9")

    data <- iNZread("enc-latin.csv")
    expect_s3_class(data, "data.frame")
})

