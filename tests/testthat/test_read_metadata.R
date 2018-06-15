context('Metadata - read')

test_that('Metadata is read correctly', {
    data <- readMetadata2('meta.csv')
    expect_is(data, 'data.frame')
    expect_equal(names(data),
        c('cellsource', 'rightfoot', 'travel', 'getlunch', 'height',
          'gender', 'age', 'year', 'armspan', 'cellcost', 'school', 'phonebill'))
    expect_equal(as.character(sapply(data, class)), 
        c('factor', 'numeric', 'factor', 'factor', 'numeric', 
          'factor', 'numeric', 'numeric', 'numeric', 'numeric', 'factor', 'numeric'))
})