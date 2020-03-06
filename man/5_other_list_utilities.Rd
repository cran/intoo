\name{5_other_list_utilities}
\alias{UNPACK}
\title{Unpack Named Lists}
\description{Copy elements from a named list into the current environment.}
\details{This function is a wrapper for the list2env function.}
\usage{
UNPACK (x)
}
\arguments{
\item{x}{A named list.}
}
\examples{
f <- function ()
    list (x=1, y=2)

UNPACK (f () )
x
y
}
