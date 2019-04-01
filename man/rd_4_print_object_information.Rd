\name{rd_4_print_object_information}
\alias{object.model}
\alias{object.summary}
\title{Print Object Information}
\description{Functions for printing objects, compactly but informatively.}
\usage{
object.model (object, value=FALSE,
    private.attributes=FALSE, public.attributes=TRUE,
    values=value, comments=FALSE, n=3L)

object.summary (object, value=TRUE,
    private.attributes=FALSE, public.attributes=TRUE,
    values=value, comments=TRUE, n=6L)
}
\arguments{
\item{object}{An object.}
\item{value}{If true, print the object's value.}
\item{private.attributes}{If true, print private attributes, regarded as attributes starting with a period.}
\item{public.attributes}{If true, print public attributes, regarded as attributes not starting with a period.}
\item{values}{If true, print the attributes' values.}
\item{comments}{If true, print the object's comment attribute, if applicable.}
\item{n}{Number of elements, lines or rows to print.}
}
\details{
These functions are similar to str().

However, currently, they don't print list values, but are more suitable for printing function objects.

They print an object with its class, length (or dimensions) and attributes, along with the class and length (or dimensions) of each attribute. Currently, they're not recursive, however, they do print the number of subattributes, if applicable. In general, special attributes (such as names) are not printed.

And they are the same as each other, except that they have different defaults.

In general, each value is printed by calling head(), so there needs to be a suitable head method for this.

Here, the length of a function is regarded as the number of formatted lines. Currently, it's possible to print S4 objects, however, the output is unlikely to be suitable. And the number of subattributes printed doesn't include special attributes, and depends on the values of private.attributes and public.attributes.
}
\examples{
#an object
object = EXTEND (1L, "something", A=1, B=1)

#set further attributes
object \%$\% A \%$\% .X = 1L
object \%$\% A \%$\% X = 1L

#a comment
comment (object) = "my object"

#print object one way
object.model (object)

#print object another way
object.summary (object)
}
