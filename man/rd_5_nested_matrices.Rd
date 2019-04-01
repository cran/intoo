\name{rd_5_nested_matrices}
\alias{P.nmatrix}
\alias{P.smatrix}
\alias{P.ht}
\title{Nested Matrices}
\description{Early prototypes for nested matrix objects.}
\usage{
P.nmatrix (x, sms)
P.smatrix (x1, x2, y1, y2)

P.ht (x, n=4L)
}
\arguments{
\item{x}{A standard matrix.}
\item{sms}{A list of P.smatrix objects.}
\item{x1}{The first row.}
\item{x2}{The last row.}
\item{y1}{The first column.}
\item{y2}{The last column.}
\item{n}{The number of rows and columns in each resulting submatrix.}
}
\details{
These functions are only early prototypes and will be changed in the future.

The P.ht function produces the head and tail of a standard matrix, both in rows and columns.
}
\examples{
x = outer (1:8, 1:8, "+")

sm.1 = P.smatrix (1, 2, 1, 2)
sm.2 = P.smatrix (1, 4, 1, 4)
sm.3 = P.smatrix (1, 6, 1, 6)
sm.4 = P.smatrix (1, 8, 1, 8)

P.nmatrix (x, list (sm.1, sm.2, sm.3, sm.4) )
}
