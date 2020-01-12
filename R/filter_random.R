#' Random sampling without replacement
#'
#' Take a specified number of groups of observations with fixed group size
#' by sampling without replacement
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param .data a dataframe to sample from
#' @param n the number of groups to generate
#' @param sample_size  the size of each group specified in \code{n}
#' @return a dataframe containing the random samples with
#'         tidyverse code attached
#' @seealso \code{\link{code}}
#' @examples
#' filtered <- filterRandom(iris, n = 5, sample_size = 3)
#' cat(code(filtered))
#' head(filtered)
#'
#' @author Owen Jin
#' @export
filterRandom <- function(.data, n, sample_size) {
    mc <- match.call()
    dataname <- mc$.data

    exp <- ~ .DATA %>%
        dplyr::sample_n(.SAMPLE_SIZE * .Nx, replace = FALSE) %>%
        dplyr::mutate(Sample.Number = factor(rep(1:.Nx, each = .SAMPLE_SIZE)))

    exp <- replaceVars(exp,
        .DATA = dataname,
        .SAMPLE_SIZE = sample_size,
        .Nx = n
    )

    interpolate(exp)
}
