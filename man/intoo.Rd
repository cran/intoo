\name{intoo}
\alias{\%$\%}
\alias{\%$\%<-}
\alias{object.info}
\title{Attribute Operators}
\description{Implements \%$\% and \%$\%<- attribute operators and an object.info() function for printing objects with attributes, succinctly but informatively.}
\usage{
object \%$\% name
object \%$\% name <- value
object.info (object,
	print.value=TRUE,
	print.value.length=TRUE,
	print.long.value=FALSE,
	print.class=TRUE,
	print.class.length=print.value.length,
	print.long.class=print.long.value,
	print.environment=FALSE,
	print.attributes=TRUE,
	print.many.attributes=TRUE,
	print.attribute.names=TRUE,
	print.attribute.classes=TRUE,
	print.attribute.lengths=print.value.length,
	print.attribute.values=TRUE,
	print.long.attribute.values=print.long.value,
	n=8L)
}
\arguments{
\item{object}{An S3 object.}
\item{name}{An attribute name.}
\item{value}{An attribute value.}
\item{print.value}{.}
\item{print.value.length}{Ignored unless print.value is true.}
\item{print.long.value}{Ignored unless print.value is true.}
\item{print.class}{.}
\item{print.class.length}{Ignored unless print.class is true.}
\item{print.long.class}{Ignored unless print.class is true.}
\item{print.environment}{.}
\item{print.attributes}{.}
\item{print.many.attributes}{Ignored unless print.attributes is true.}
\item{print.attribute.names}{Ignored unless print.attributes is true.}
\item{print.attribute.classes}{Ignored unless print.attributes and print.attribute.names are true.}
\item{print.attribute.lengths}{Ignored unless print.attributes and print.attribute.names are true. Also, prints the length of the attribute's attributes. Currently, including any class attributes, however, this may be changed.}
\item{print.attribute.values}{Ignored unless print.attributes is true.}
\item{print.long.attribute.values}{Ignored unless print.attributes and print.attribute.values are true.}
\item{n}{.}
}
\details{Note that when printing attributes (or printing the length of an attribute's attributes), special attributes (including class, dim, srcref and .Environment) are removed.}
\examples{
obj1 = "My First Object"

obj1 \%$\% obj2a = "My Second Object"
obj1 \%$\% obj2b = 1:20
obj1 \%$\% obj2c = function (x) {x ^ 2}
obj1 \%$\% obj2d = rnorm (80)

obj1 \%$\% obj2d \%$\% obj3 = "Another Object"

obj1 \%$\% obj2d \%$\% obj3 \%$\% obj4 = "And Another Object"

obj1 \%$\% obj2a

object.info (obj1)
object.info (obj1 \%$\% obj2d)
}
