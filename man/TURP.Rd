\name{TURP}
\alias{TURP}
\docType{data}
\title{TURP Dataset}
\description{TURP data}
\usage{
data(TURP)
}
\format{
    A \code{data.frame} on 61 hospital service areas (HSA) in Switzerland
    with data on the number of transurethral resections of the prostate in
    2014.
    \describe{
        \item{HSA}{\code{[integer]} identifier of HSA.}
        \item{yi}{\code{[numeric]} relative hospitalization rate, defined as
            \eqn{(o_i - e_i) / e_i}.}
        \item{di}{\code{[numeric]} variance of relative hospitalization rate,
            defined as \eqn{o_i/e_i^2}.}
        \item{oi}{\code{[numeric]} counts of hospitalizations in HSA.}
        \item{ei}{\code{[numeric]} expected counts of hospitalizations in HSA.}
        \item{ni}{\code{[numeric]} population size of HSA.}
    }
}
\details{
Treatment data (in-patient care) on the number of transurethral resections
of the prostate in 2014 for 61 hospital service areas (HSA) in Switzerland.
}
\keyword{datasets}
