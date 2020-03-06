\name{4_print_objects}
\alias{object.model}
\alias{object.summary}
\title{Print Object Information}
\description{Functions for printing objects, compactly but informatively.}
\usage{
object.model (object, \dots,
    value=FALSE,
    private.attributes=TRUE, public.attributes=TRUE,
    attribute.values=value, comment=FALSE, n=3)

object.summary (object,\dots,
    value=TRUE,
    private.attributes=FALSE, public.attributes=TRUE,
    attribute.values=value, comment=TRUE, n=6)
}
\arguments{
\item{object}{An object.}
\item{value}{If true, print the object's value.}
\item{private.attributes}{If true, print private attributes, regarded as attributes starting with a period.}
\item{public.attributes}{If true, print public attributes, regarded as attributes not starting with a period.}
\item{attribute.values}{If true, print the attributes' values.}
\item{comment}{If true, print the object's comment attribute, if applicable.}
\item{n}{Number of elements, lines or rows to print.}
\item{\dots}{Ignored.}
}
\details{
These functions are the same, except for their defaults.

By default, the object.model function prints a compact representation of an object.\cr
And by default, the object.summary function prints a semi-compact representation of an object.

Output contains non-recursive information regarding attributes.\cr
In general, language-related attributes (such as names) are excluded from the list of attributes.

The object.summary function has output intermediate between the str and head functions, and is particularly suitable for printing functions bundled with data.
}
\examples{
#function object
#with private and public attributes
f <- function (x)
{   . = THAT ()
    with (.,
        paste (A, B, x, sep=":") )
}
f <- EXTEND (f, "concat", .X=10, A="A", B="B")

#compact print
object.model (f)

#semi-compact print
object.summary (f)
}
