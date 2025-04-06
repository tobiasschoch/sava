\name{SCV}
\alias{SCV}
\alias{predict.sava_scv}
\title{Systematic Component of Variation}
\description{
Estimator of the systematic component of variation (SCV) of McPherson et al.
(1982)
}
\usage{
SCV(oi, ei, approx = TRUE)

# Utility functions
\method{predict}{sava_scv}(object, ...)
}
\arguments{
    \item{oi}{\code{[numeric vector]} observed counts.}
    \item{ei}{\code{[numeric vector]} expected counts.}
    \item{approx}{\code{[logical]} if \code{TRUE}, then \code{log(o[i] / e[i])}
        is approximated by \code{(o[i] - e[i]) / e[i]} (default: \code{FALSE});
        see Sect. details.}
    \item{object}{an object of class \code{sava_scv}.}
    \item{...}{additional arguments.}
}
\details{
McPherson et al. (1982, p. 1314) define \eqn{y_i = \log (o_i / e_i)}{y[i] =
    log(o[i] / e[i])}, where \eqn{o_i}{o[i]} and \eqn{e_i}{e[i]} denote,
respectively, the number of observed and expected counts. The number of
expected cases must be strictly larger than zero. The SCV is then computed
based on \eqn{y_i^2}{y[i]^2}. The above definition of \eqn{y_i}{y[i]} can
problematic if zero cases were observed in one or more areas because
\eqn{y_i}{y[i]} will then be negative infinity. As a consequence, SCV will be
negative infinity as well. Diehr et al. (1990, p. 754) define instead \eqn{y_i
    = (o_i - e_i) / e_i}{y[i] = (o[i] - e[i]) / e[i]}, which is an
approximation that does not suffer from the issue. This definition will be used
if \code{SCV()} is called with argument \code{approx = TRUE}.

The SCV can (in principle) be negative if the sum of the \eqn{y_i^2}{y[i]^2} is
smaller than the sum of the \eqn{1/e_i}{1/e[i]}. If this happens, \code{SCV()}
returns \code{NA} because the SCV as a measure of variation is not well defined
in this case.

}
\value{
An object of class \code{sava_scv}
}
\references{
McPherson, K., Wennberg, J. E., Hovind, O. B. and Clifford, P. (1982).
    Small-Area Variations in the Use of Common Surgical Procedures: An
    International Comparison of New England, England, and Norway.
    \emph{New England Journal of Medicine}, \bold{307}, 1310--1314.
    \doi{10.1056/NEJM198211183072104}

Diehr, P., Cain, K., Connell, F. and Volinn, E. (1990). What is too much
    variation? The null hypothesis in small- area analysis.
    \emph{Health Service Research}, \bold{24}, 741--771.
}
\author{
Tobias Schoch
}
\seealso{
\code{\link[=EBnormal0]{EBnormal0()}},
\code{\link[=EBpoisson0]{EBpoisson0()}}
}
\examples{
data("TURP")

# estimate SCV
est <- SCV(TURP$oi, TURP$ei)
est

# predict rates
predict(est)
}
