context("Transform variables")

test_that("Reciprocal and square transformations work", {
    recip <- transformVar(iris, "Sepal.Length", "reciprocal", name = "x")
    expect_equal(recip$x, 1 / iris$Sepal.Length)
    expect_equivalent(
        eval(parse(text = code(recip))),
        recip
    )

    square <- transformVar(iris, "Sepal.Length", "square", name = "x")
    expect_equal(square$x, iris$Sepal.Length^2)
    expect_equivalent(
        eval(parse(text = code(square))),
        square
    )
})
