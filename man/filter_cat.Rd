% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R
\name{filter_cat}
\alias{filter_cat}
\title{Filter data by levels of categorical variables}
\usage{
filter_cat(data, var, levels)
}
\arguments{
\item{data}{A dataframe or survey design object to be filtered.}

\item{var}{The name of the column in \code{data} to be filtered by.}

\item{levels}{A character vector of levels in \code{var} to keep.}
}
\value{
A filtered dataframe with the tidyverse code attached.
}
\description{
This function filters a dataframe or survey design object by keeping only
the rows where a specified categorical variable matches one of the given
levels. The resulting filtered dataframe is returned, along with the
tidyverse code used to generate it.
}
\examples{
filtered <- filter_cat(iris,
    var = "Species",
    levels = c("versicolor", "virginica")
)
cat(code(filtered))
head(filtered)

}
\seealso{
\code{\link{code}}
}
\author{
Owen Jin, Zhaoming Su
}
