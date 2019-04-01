\name{rd_2_convenience_functions_1}
\alias{THIS}
\alias{THAT}
\title{Convenience Functions 1}
\description{Convenience functions to get the system function, or its attributes.}
\usage{
THIS ()
THAT ()
}
\details{
These functions are simply wrappers for standard R functions.

THIS() returns the current system function, and THAT() returns the current system function's attributes.

Note that they should be evaluated inside the function calling them.
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
