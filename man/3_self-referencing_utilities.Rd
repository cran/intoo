\name{3_self-referencing_utilities}
\alias{THIS}
\alias{THAT}
\alias{THEN}
\title{Self-Referencing Utilities}
\description{Convenience functions to get the system function, its attributes or environment.}
\usage{
THIS ()

THAT ()
THEN ()
}
\details{
These functions are wrappers for standard R functions.

THIS returns the system function, that is, the current function that's being called.

THAT returns the system function's attributes and THEN returns the system function's environment.\cr
Note that THAT and THEN are contractions of TH(is) AT(tributes) and TH(is) (EN)vironment.

They need to be evaluated inside the function where they're defined.\cr
(This is important, please refer to the examples).

It may be useful to combine the with/THAT functions.\cr
(Also, please refer to the examples).

In general, the THEN function isn't necessary.\cr
(Because R uses lexical scoping, and functions can be called with standard syntax, using objects contained in their environments).
}
\examples{
#good
#(THIS evaluated *before* do.something)
f <- function (x)
{   this <- THIS ()
    do.something (this, x)
}

#bad
#(THIS evaluated *in* do.something)
f <- function (x)
    do.something (THIS (), x)

#combined with/THAT example
f <- function (x)
{   . <- THAT ()
    with (.,
        do.something (attr.1, attr.2, x) )
}
}
