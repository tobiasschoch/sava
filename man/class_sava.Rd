\name{sava_utilities}
\alias{params}
\alias{params.sava}
\alias{print.sava}
\alias{mse}
\alias{jackknife}
\alias{EQ}
\alias{QSR}
\title{Utility Functions for Class sava}
\description{
Utility functions for instances of class \code{sava}
}
\usage{
params(object, ...)
mse(object, ...)
jackknife(object, ...)
EQ(object, q = 0.2)

# Utility functions
\method{print}{sava}(x, digits = max(1L, getOption("digits") - 2L), ...)
\method{params}{sava}(object, ...)
}
\arguments{
    \item{x}{an object of class \code{sava}.}
    \item{object}{an object of class \code{sava}.}
    \item{digits}{\code{[integer]} minimal number of significant digits.}
    \item{q}{\code{[numeric]} quantile}
    \item{...}{additional arguments.}
}
\details{
Function \code{params()} extracts the estimated parameters; the functions
\code{mse()} and \code{jackknife()} compute, respectively, the mean square
prediction error and the jackknife variance of the (variance) parameter.
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

# estimate SCV and extract estimated parameters
params(SCV(TURP$oi, TURP$ei))

# estimate Poisson-gamma model
est <- EBpoisson0(TURP$oi, TURP$ei)

# compute extremal quotient
EQ(est)
}
