library(magrittr)

test_that("Cody tidying does not break code", {
    x <- readLines("messy_gapminder.txt")
    z <- tidy_all_code(x)
    tf <- tempfile()
    on.exit(unlink(tf))
    writeLines(x, tf)

    e <- new.env()
    source(tf, e)
    d1 <- e$gapminder_2008_ex
    rm(e)

    writeLines(z, tf)
    e <- new.env()
    source(tf, e)
    d2 <- e$gapminder_2008_ex
    rm(e)

    expect_equal(d1, d2)
})

test_that("Code writing can remove library calls", {
    x <- readLines("messy_gapminder.txt")
    z <- tidy_all_code(x, incl_library = FALSE)
    expect_false(any(grepl("tibble::", z)))
})

test_that("Cody tidying does not break code 2", {
    x <- readLines("messy_census.txt")
    z <- tidy_all_code(x)
    tf <- tempfile()
    on.exit(unlink(tf))
    writeLines(x, tf)

    e <- new.env()
    source(tf, e)
    d1 <- e$census.at.school.500_ex
    rm(e)

    writeLines(z, tf)
    e <- new.env()
    source(tf, e)
    d2 <- e$census.at.school.500_ex
    rm(e)

    expect_equal(d1, d2)
})

test_that("print_code works OK", {
    dat <- filterRows(iris, c(10, 20, 30))
    expect_equal(
        eval(parse(text = capture.output(print_code(dat)))),
        dat,
        ignore_attr = TRUE
    )

    expect_message(
        print_code(iris),
        "No code attached to this object."
    )
})

test_that("tidying from file works OK", {
    x <- tidy_all_code("messy_gapminder.txt")
    expect_true(length(x) > 0)
})
