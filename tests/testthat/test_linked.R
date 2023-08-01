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
iris_extra <- data.frame(
    id = 1:2,
    type = c("Fluffy", "Hard")
)
iris_schema <- list(
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
)

t0 <- tempfile(fileext = ".sql")
t1 <- tempfile(fileext = ".csv")
t2 <- tempfile(fileext = ".csv")
t3 <- tempfile(fileext = ".csv")
t4 <- tempfile(fileext = ".inzlnk")
on.exit(unlink(c(t1, t2, t3, t4)))

write.csv(iris_species, file = t1, row.names = FALSE, quote = FALSE)
write.csv(iris_data, file = t2, row.names = FALSE, quote = FALSE)
write.csv(iris_extra, file = t3, row.names = FALSE, quote = FALSE)
writeLines(
    sprintf(
        "files:
    iris_species: %s
    iris_data: %s
    iris_extra: %s
schema:
  iris_data:
    links_to:
      iris_species: species_id
  iris_species:
    links_to:
      iris_extra:
        type_id: id
", t1, t2, t3
    ),
    t4
)

test_that("Linked data loading", {
    con <- DBI::dbConnect(RSQLite::SQLite(), t0)
    on.exit({
        DBI::dbDisconnect(con)
        unlink(t0)
    })

    dl <- load_linked(
        c(iris_species = t1, iris_data = t2, iris_extra = t3),
        schema = iris_schema,
        con = con,
        name = "iris",
        keep_con = TRUE
    )
    expect_s3_class(dl, "inzdf_db")
})

test_that("Non-db linking", {
    dl <- load_linked(
        c(iris_species = t1, iris_data = t2, iris_extra = t3),
        schema = iris_schema,
        name = "iris"
    )
    expect_s3_class(dl, "inzdf_tbl_df")
})

test_that("Link spec file", {
    con <- DBI::dbConnect(RSQLite::SQLite(), t0)
    on.exit({
        DBI::dbDisconnect(con)
        unlink(t0)
    })

    dl <- read_link_spec(t4)
    expect_s3_class(dl, "inzlnk_spec")
    expect_equal(dl$schema, iris_schema)

    d <- load_linked(dl, con = con, keep_con = TRUE)
    expect_s3_class(d, "inzdf_db")
    expect_equal(dim(d), c(150L, 9L))
})

test_that("Dicionaries load", {
    tc <- tempfile(fileext = ".inzlnk")
    con <- DBI::dbConnect(RSQLite::SQLite(), t0)
    on.exit({
        DBI::dbDisconnect(con)
        unlink(tc)
        unlink(t0)
    })
    writeLines(
        "files:
    cas: ./cas500_coded.csv
schema:
    cas:
dictionary:
    file: ./casdict.csv
    name: variable
",
        tc
    )

    d <- load_linked(tc, con = con, keep_con = TRUE)
    expect_equal(dim(d), c(500L, 10L))
    expect_s3_class(d[["travel"]], "factor")
    expect_equal(levels(d[["gender"]]), c("female", "male"))
})
