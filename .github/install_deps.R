# R script
github_deps <- c(
    "tmelliott/surveyspec@develop"
)

OS <- Sys.getenv("OS_TYPE")
if (OS == "Windows" && !requireNamespace("RCurl", quietly = TRUE)) {
    install.packages("RCurl", repos = "https://cran.r-project.org")
}

options(
    repos = c(
        RSPM = Sys.getenv("RSPM"),
        CRAN = "https://cloud.r-project.org"
    ),
    install.packages.compile.from.source = "never"
)

if (OS == "Windows" && packageVersion("Matrix") < "1.6-0") {
    install.packages("Matrix", type = "source")
}

if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}

remotes::install_github(github_deps,
    INSTALL_opts = c("--no-multiarch")
)
remotes::install_deps(
    dependencies = TRUE,
    INSTALL_opts = c("--no-multiarch")
)
remotes::install_cran("rcmdcheck",
    INSTALL_opts = c("--no-multiarch")
)
