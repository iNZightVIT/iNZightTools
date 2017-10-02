##' Convert a numeric variable to Categorical (factor)
##'
##' @title Convert to Categorical
##' @param .dataset the dataset to use
##' @param var character, name of the numeric variable
##' @param name name of the created variable (defaults to {original}.cat)
##' @return a dataframe with the new variable, along with the code used to generate it
##' @author Tom Elliott
##'
##' @import tibble
##' @import magrittr
##' @export
numToCat <- function(.dataset, var, name = sprintf("%s.cat", var)) {
    exp <- as.formula(sprintf(
        "~.dataset %%>%% tibble::add_column(%s = as.factor(%s), .after = var)",
        name, paste0(".dataset$", var)))
    interpolate(exp, var = var,
                comment = sprintf("Convert %s to categorical", var))
}


##' Transform a numeric variable
##'
##' @title Transform Numeric Variable
##' @param .dataset the dataset to use
##' @param var character, name of the numeric variable
##' @param fn character, name of the function to call
##' @param name name of the created variable (defaults to {original}.{fn})
##' @return a dataframe with the new variable(, along with the code used to generate it)
##'
##' @author Tom Elliott
##' @export
transform <- function(.dataset, var, fn, name = sprintf("%s.%s", fn, var)) {
    x <- switch(fn,
                "square" = .dataset[[var]]^2,
                "recip" = 1 / .dataset[[var]],
                do.call(fn, list(.dataset[[var]])))
    w <- which(names(.dataset) == var)
    data<- .dataset[, 1:w]
    data[[name]] <- round(x, 3)
    if (w < ncol(.dataset))
        data <- cbind(data, .dataset[, -(1:w)])

    data
}
