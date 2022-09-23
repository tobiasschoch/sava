\name{sava_utilities}
\alias{params}
\alias{params.sava}
\alias{print.sava}
\title{Utility Functions for Class sava}
\description{
Utility functions for instances of class \code{sava}
}
\usage{
params(object, ...)

# Utility functions
\method{print}{sava}(x, digits = max(1L, getOption("digits") - 2L), ...)
\method{params}{sava}(object, ...)
}
\arguments{
    \item{x}{an object of class \code{sava}.}
    \item{object}{an object of class \code{sava}.}
    \item{digits}{\code{[integer]} minimal number of significant digits.}
    \item{...}{additional arguments.}
}
\details{
Function \code{params()} extracts the estimated parameters.
}
\author{
Tobias Schoch
}
\seealso{
\code{\link[=SCV]{SCV()}}, \code{\link[=EBnormal0]{EBnormal0()}},
\code{\link[=EBpoisson0]{EBpoisson0()}}
}
\examples{
data("TURP")

% estimate SCV and extract estimated parameters
params(SCV(TURP$oi, TURP$ei))
}
