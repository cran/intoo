\name{2_constructor_utilities}
\alias{LIST}
\alias{EXTEND}
\title{Constructor Utilities}
\description{Functions for constructing named lists and S3 objects.}
\usage{
LIST (\dots)
EXTEND (object, class, \dots)
}
\arguments{
\item{object}{An object.}
\item{class}{String, giving the class name.\cr Can be a character vector, in which case, subclasses precede superclasses.\cr Also, can be NULL or missing.}
\item{\dots}{A list of name=value pairs, or objects.}
}
\details{
The LIST function constructs/returns a named list, automatically naming it's elements, if names aren't supplied.

The EXTEND function constructs/returns an S3 object, by prepending the (optional) new class to the existing classes, and then assigning attributes.

Note that it's possible to combine the with/EXTEND functions, to take attribute values from a named list.
}
\examples{
object <- x <- y <- z <- 1

LIST (x, y, z=0, k=z)

EXTEND (object, "something", x, y, z=0, k=z)
}
