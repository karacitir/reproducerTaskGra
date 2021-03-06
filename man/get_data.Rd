% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_functions.R
\name{get_data}
\alias{get_data}
\title{Get experiment dataset}
\usage{
get_data(columns = c())
}
\arguments{
\item{columns}{a character vector containing the column names to be returned}
}
\value{
a dataframe contating the selected columns of the experiment dataset
}
\description{
Get experiment dataset
}
\examples{
get_data()
get_data(c("TASK_GRA", "COMP"))
}
