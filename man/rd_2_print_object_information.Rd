\name{rd_2_print_object_information}
\alias{object.model}
\alias{object.summary}
\title{Print Object Information}
\description{Functions for printing arbitrary S3 objects, compactly but informatively.}
\usage{
object.model (object, value=FALSE,
    private.attributes=FALSE, public.attributes=TRUE,
    values=value, comments=FALSE, n=3)
object.summary (object, value=TRUE,
    private.attributes=FALSE, public.attributes=TRUE,
    values=value, comments=TRUE, n=6)
}
\arguments{
\item{object}{An arbitrary S3 object.}
\item{value}{If true, print the object's value.}
\item{private.attributes}{If true, print private attributes, regarded as attributes starting with a period.}
\item{public.attributes}{If true, print public attributes, regarded as attributes not starting with a period.}
\item{values}{If true, print the attributes' values.}
\item{comments}{If true, print the object's comments attributes, if applicable.}
\item{n}{Number of elements, lines or rows to print.}
}
\details{
These functions are the same as each other, except that they have different defaults. 

They print an object with it's attributes, along with the class and length (or dimensions) for each one. This is similar to str(), however, they're designed more for function objects, and print attributes differently. Currently, they're not recursive, however, they do print the number of subattributes, if applicable.

Special attributes (such as class and names) are not printed.

Note that the number of subattributes printed doesn't include special attributes, and depends on the values of private.attributes and public.attributes.
}
\examples{
#an object
object = 1L

#set attributes
object \%$\% A = 1L
object \%$\% B = function () 1L
object \%$\% A \%$\% .X = 1L
object \%$\% A \%$\% X = 1L

#a comment
comment (object) = "my object"

#print object
object.model (object)

#print object again
object.summary (object)
}
