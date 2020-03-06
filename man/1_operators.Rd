\name{1_operators}
\alias{\%$\%}
\alias{\%$\%<-}
\alias{\%@\%}
\alias{\%@\%<-}
\title{Attribute and Environment Member Operators}
\description{Binary operators to get and set object attributes and environment members.}
\usage{
#object attributes
object \%$\% name
object \%$\% name <- value

#environment members
object \%@\% name
object \%@\% name <- value
}
\arguments{
\item{object}{An object.}
\item{name}{An attribute's name.}
\item{value}{An attribute's value.}
}
\details{
The attribute operators are equivalent to calling the attr and attr<- functions.

Note that currently, they need to be inside parenthesis, for subsetting and function calls.

Also note that attribute names need to be quoted inside R packages.
}
\examples{
#simple object
object <- 0

#set attributes
object \%$\% A <- 1
object \%$\% B <- 2
object \%$\% A \%$\% I <- 10
object \%$\% A \%$\% J <- 20

#get attributes
object \%$\% A
object \%$\% B
object \%$\% A \%$\% I
object \%$\% A \%$\% J

#quoted version
#(for R packages)
object \%$\% "A"
object \%$\% "B"

#this doesn't work
#object \%$\% A [1]

#however, this does work
(object \%$\% A) [1]

#function object
#(using lexical scope)
g <- function ()
{   A <- 2
    B <- 2
    function (x)
        A * B + x
}
f <- g ()

#get environment members
f \%@\% A
f \%@\% B
}
