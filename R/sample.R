#' Random sampling without replacement
#'
#' Take a specified number of groups of observations with fixed group size
#' by sampling without replacement
#' and returns the result along with tidyverse code used to generate it.
#'
#' @param data a dataframe to sample from
#' @param n the number of groups to generate
#' @param sample_size  the size of each group specified in \code{n}
#' @return a dataframe containing the random samples with
#'         tidyverse code attached
#' @seealso \code{\link{code}}
#' @rdname random_sample
#' @examples
#' rs <- random_sample(iris, n = 5, sample_size = 3)
#' cat(code(rs))
#' head(rs)
#'
#' @author Owen Jin, Zhaoming Su
#' @export
#' @md
random_sample <- function(data, n, sample_size) {
    expr <- rlang::enexpr(data)
    if (is_survey(data)) {
        rlang::abort("Survey data cannot be sampled at this stage.")
    }
    expr <- rlang::expr(!!expr %>%
        dplyr::slice_sample(n = !!n * !!sample_size) %>%
        dplyr::mutate(.group = factor(rep(seq_len(!!n), each = !!sample_size))))
    eval_code(expr)
}
