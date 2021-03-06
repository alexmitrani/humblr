% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dropr.R
\name{dropr}
\alias{dropr}
\title{drops a specified list of variables and keeps the rest}
\usage{
dropr(mydf, ...)
}
\arguments{
\item{mydf}{the dataframe to be modified.}

\item{...}{a list of the variables to be dropped.}
}
\value{

}
\description{
dropr is used to drop a list of variables from a dataframe (and to keep the rest).  Inspired by the "drop" command in Stata.
}
\details{
dropr is used internally by the fsm package.
}
\examples{
mydf <- mtcars
mydf <- dropr(mydf, "am", "gear", "carb")
mydf

}
