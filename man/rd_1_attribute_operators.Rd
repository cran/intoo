\name{rd_1_attribute_operators}
\alias{\%$\%}
\alias{\%$\%<-}
\title{Attribute Operators}
\description{Operators to get and set attributes.}
\usage{
object \%$\% name
object \%$\% name <- value
}
\arguments{
\item{object}{An object.}
\item{name}{An attribute's name.}
\item{value}{An attribute's value.}
}
\details{
These operators are equivalent to calling attr() or attr()<-.

Note that currently, they need to be inside parenthesis, for subsetting and function calls.
}
\examples{
#an object
object = 1L

#set attributes
object \%$\% A = 1L
object \%$\% B = function () 1L
object \%$\% A \%$\% X = 1L

#get attributes
object \%$\% A
object \%$\% B
object \%$\% A \%$\% X

#this doesn't work
#object \%$\% A [1]
#object \%$\% B ()

#however, this does work
(object \%$\% A) [1]
(object \%$\% B) ()
}
