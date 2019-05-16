\name{rd_3_convenience_functions_2}
\alias{THIS}
\alias{THAT}
\alias{THEN}
\title{Convenience Functions 1}
\description{Convenience functions to get the system function, its attributes or environment.}
\usage{
THIS ()
THAT ()
THEN ()
}
\details{
These functions are simply wrappers for standard R functions.

THIS() returns the system function, THAT() returns the system function's attributes and THEN() returns the system function's environment.

Note that THAT is derived from TH(is) AT(tributes) and THEN from TH(is) (EN)vironment. Also, note that they should be evaluated inside the function containing them.
}
\examples{
#good
f = function ()
{   this = THIS ()
    do.something (this)
}

#bad
f = function ()
    do.something (THIS () )
}
