test_that("Data.frame supported", {
    d <- inzdf(iris)
    expect_s3_class(d, "inzdf_tbl_df")
})

test_that("SQLite database supported", {
    skip_if_not_installed("RSQLite")

    db <- tempfile(fileext = ".db")
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    on.exit({
        DBI::dbDisconnect(con)
        unlink(db)
    })
    DBI::dbWriteTable(con, "iris", iris)

    d <- inzdf(con, "iris", keep_con = TRUE)
    expect_s3_class(d, "inzdf_db")

    expect_s3_class(as.data.frame(d), "data.frame")
})

test_that("Linked data in database supported", {
    skip_if_not_installed("RSQLite")

    iris_species <- data.frame(
        species_id = 1:3,
        species_name = levels(iris$Species),
        type_id = c(1L, 1L, 2L)
    )
    iris_data <- iris %>%
        dplyr::mutate(
            id = seq_len(dplyr::n()),
            species_id = as.integer(iris$Species),
            Species = NULL
        )

    db <- tempfile(fileext = ".db")
    con <- DBI::dbConnect(RSQLite::SQLite(), db)
    on.exit({
        DBI::dbDisconnect(con)
        unlink(db)
    })
    DBI::dbWriteTable(con, "iris_species", iris_species)
    DBI::dbWriteTable(con, "iris_data", iris_data)
    DBI::dbWriteTable(
        con, "iris_extra",
        data.frame(
            id = 1:2,
            type = c("Fluffy", "Hard")
        )
    )

    d <- inzdf(con,
        "iris_linked",
        schema = list(
            iris_data = list(
                links_to = list(
                    iris_species = "species_id"
                )
            ),
            iris_species = list(
                links_to = list(
                    iris_extra = c("type_id" = "id")
                )
            )
        ),
        keep_con = TRUE
    )

    ds <- d %>% select(species_name, type, Sepal.Length)
    expect_s3_class(ds, "tbl_lazy")
    expect_s3_class(as_tibble(ds), "tbl_df")
    expect_equal(dim(as_tibble(ds)), c(150L, 3L))
})
