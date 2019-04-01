\name{rd_3_convenience_functions_2}
\alias{LIST}
\alias{EXTEND}
\title{Convenience Functions 2}
\description{Functions for constructing named lists or S3 objects.}
\usage{
LIST (\dots)
EXTEND (object, class, \dots)
}
\arguments{
\item{object}{An object.}
\item{class}{A class name.}
\item{\dots}{A list of values.}
}
\details{
The LIST() function constructs a named list, automatically naming it's elements, if names aren't supplied.

The EXTEND() function constructs an S3 object, by prepending the new class to the existing class, and then assigning attributes.

Note that extending vectors needs to be done carefully, because some operations change their mode.
}
\examples{
object = x = y = z = 1L

#names = c ("x", "y", "other")
LIST (x, y, other=z)

#class = c ("something", "integer")
#other attribute names = c ("x", "y", "other")
EXTEND (object, "something", x, y, other=z)
}
