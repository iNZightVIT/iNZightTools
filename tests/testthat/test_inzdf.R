test_that("Data.frame supported", {
    d <- inzdf(iris)
    expect_s3_class(d, "inzdf_tbl_df")
})

test_that("SQLite database supported", {
    skip_if_not_installed("RSQLite")

    db <- tempfile(fileext = ".db")
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    on.exit({DBI::dbDisconnect(con); unlink(db)})
    DBI::dbWriteTable(con, "iris", iris)

    d <- inzdf(con, "iris")
    expect_s3_class(d, "inzdf_db")
})
