\name{rd_6_nested_matrix_methods}
\alias{dim.P.nmatrix}
\alias{print.P.nmatrix}
\alias{head.P.nmatrix}
\alias{tail.P.nmatrix}
\alias{format.P.nmatrix}
\alias{print.P.smatrix}
\title{Nested Matrix Methods}
\description{Dim, print, head, tail and format methods for nested matrix objects.}
\usage{
\method{dim}{P.nmatrix}(x)
\method{print}{P.nmatrix}(x, \dots)
\method{head}{P.nmatrix}(x, n=6L, \dots)
\method{tail}{P.nmatrix}(x, n=6L, \dots)
\method{format}{P.nmatrix}(x, reverse=FALSE, na.string="", \dots)

\method{print}{P.smatrix}(x, \dots)
}
\arguments{
\item{x}{A P.nmatrix object, or a P.smatrix object in the case of P.smatrix method.}
\item{n}{The number of rows to print.}
\item{reverse}{Refer to details.}
\item{na.string}{What to format NAs as.}
\item{\dots}{For the P.nmatrix methods, further arguments for format. For other methods, ignored.}
}
\details{
The print, head and tail methods, call the format method first.

The format method produces a character matrix, with formatting, showing the submatrices.

By default, submatrices listed last overwrite submatrices listed first, if their borders overlap.
}
\examples{
x = outer (1:8, 1:8, "+")

sm.1 = P.smatrix (2, 5, 2, 5)
sm.2 = P.smatrix (4, 7, 4, 7)

nm = P.nmatrix (x, list (sm.1, sm.2) )

print (nm)
print (nm, reverse=TRUE)
}
