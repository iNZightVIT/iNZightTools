context('Metadata - read')

test_that('Metadata is read correctly', {
    data <- smart_read('meta.csv')
    expect_is(data, 'data.frame')
    expect_equal(names(data),
        c('cellsource', 'rightfoot', 'travel', 'getlunch', 'height',
          'gender', 'age', 'year', 'armspan', 'cellcost', 'school', 'phonebill'))
    expect_equal(as.character(sapply(data, class)),
        c('factor', 'numeric', 'factor', 'factor', 'numeric',
          'factor', 'numeric', 'numeric', 'numeric', 'numeric', 'factor', 'numeric'))
})

test_that('Name is read', {
    expect_equal(attr(smart_read('meta.csv'), 'name'), 'Census at School (subset)')
    expect_equal(attr(smart_read('meta.txt'), 'name'), 'meta')
})

test_that("Factor level orders are respected", {
    data <- smart_read('meta.csv')
    # data <- smart_read('tests/testthat/meta.csv')
    expect_equal(levels(data$gender), c("male", "female"))
})

test_that("First row is read if no title/description given", {
    expect_equal(length(readMetaComments("meta.txt")$columns), 2)
    expect_equal(levels(smart_read("meta.txt")$gender), c("male", "female"))
})

test_that("Special characters are removed from factor levels", {
    expect_equal(
        levels(smart_read('meta_chars.csv')$var2),
        c("maori", "english", "francais")
    )
})

test_that("Multiple response data read correctly", {
    m <- smart_read("meta_multi.csv")
    expect_equal(
        names(m),
        c(
            "age", "tech_phone", "tech_pc", "tech_tablet", "tech_watch",
            "lunch_home", "lunch_shop", "lunch_school", "lunch_none"
        )
    )
    expect_equal(m$tech_phone, c(1L, 1L, 1L, 0L))
})


## gist
library(tibble)
library(dplyr)
library(tidyr)

d <- tibble(
    x = 1:4,
    y = list(
        c("a", "b"),
        c("a", "c", "d"),
        c("b", "c"),
        c("d")
    ),
    z = list(
        c("one", "two", "four"),
        c("three"),
        c("two", "three"),
        c("three", "four")
    )
)

mutate(d,
    y_a = sapply(y, function(z) as.integer("a" %in% z)),
    y_b = sapply(y, function(z) as.integer("b" %in% z)),
    y_c = sapply(y, function(z) as.integer("c" %in% z)),
    y_d = sapply(y, function(z) as.integer("d" %in% z)),
    y = NULL
)

d %>%
    unnest(y) %>%
        mutate(n = 1) %>%
        pivot_wider(names_from = y, values_from = n, values_fill = 0, names_prefix = "y_") %>%
    unnest(z) %>%
        mutate(n = 1) %>%
        pivot_wider(names_from = z, values_from = n, values_fill = 0, names_prefix = "z_")
