context('Metadata - read')

test_that('Metadata is read correctly', {
    data <- readMetadata2('meta.csv')
    expect_is(data, 'data.frame')
    expect_equal(as.character(sapply(data, class)), 
        c('factor', 'numeric', 'factor', 'factor', 'numeric', 'factor', 'integer', 'integer', 'numeric', 'numeric'))
})