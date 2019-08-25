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
