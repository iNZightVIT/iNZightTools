##' Convert a numeric variable to Categorical (factor)
##'
##' @title Convert to Categorical
##' @param .dataset the dataset to use
##' @param var character, name of the numeric variable
##' @param name name of the created variable (defaults to {original}.cat)
##' @return a dataframe with the new variable, along with the code used to generate it
##' @author Tom Elliott
##'
##' @export
numToCat <- function(.dataset, var, name = sprintf("%s.cat", var)) {
    exp <- as.formula(sprintf(
        ".dataset %%>%% tibble::add_column(%s = as.factor(%s), .after = varname)",
    name, paste0(".dataset$", orgVar)))

    interpolate(exp, var = varData, varname = orgVar)
}
