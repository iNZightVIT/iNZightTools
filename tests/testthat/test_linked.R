iris_species <- data.frame(
    species_id = 1:3,
    species_name = levels(iris$Species),
    type_id = c(1L, 1L, 2L)
)
iris_data <- iris %>%
    dplyr::mutate(
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
on.exit(unlink(c(t0, t1, t2, t3)))

con <- DBI::dbConnect(RSQLite::SQLite(), t0)
on.exit(DBI::dbDisconnect(con), add = TRUE, after = FALSE)

write.csv(iris_species, file = t1, row.names = FALSE, quote = FALSE)
write.csv(iris_data, file = t2, row.names = FALSE, quote = FALSE)
write.csv(iris_extra, file = t3, row.names = FALSE, quote = FALSE)

test_that("Link spec", {
    dl <- load_linked(
        c(iris_species = t1, iris_data = t2, iris_extra = t3),
        schema = iris_schema,
        con = con,
        name = "iris"
    )
    expect_s3_class(dl, "inzdf_db")
})
