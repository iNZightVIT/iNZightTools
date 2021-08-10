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
