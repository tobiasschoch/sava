\name{freeman_tukey}
\alias{ft}
\alias{ft_inv}
\alias{print.ft_trans}
\title{Freeman--Tukey Transforms}
\description{
Freeman--Tukey square root and double arcsine transform (and inverse
transforms)
}
\usage{
ft(x, size = NULL, type = c("root", "arcsin"))
ft_inv(x)

\method{print}{ft_trans}(x, ...)
}
\arguments{
    \item{x}{\code{[numeric vector]} data.}
    \item{size}{\code{[numeric vector]} size data of the same length as
        \code{x}.}
    \item{type}{\code{[character]} type of transform: \code{"root"} for the
        square root transform or \code{"arcsin"} for the double arcsine
        transform.}
    \item{...}{additional arguments.}
}
\details{
The double arcsine transform and the square root transform are, respectively,
variance stabilizing transforms for binomial and Poisson models; see Freeman
and Tukey (1950).
    \describe{
            \item{Double arcsine transform (binomial)}{
            Consider the binomial model. Let \eqn{x}{x} denote the number of
            successes in a sequence of \code{size} trials. Our shorthand
            notation for \code{size} is \eqn{s}{s}. The transform is
            implemented as

            \deqn{f(x, s) = \arcsin\big(x / (s + 1)\big) + \arcsin\big((x + 1)
                / (s + 1)\big).}{f(x, s) = arcsin(x / (s + 1)) + arcsin((x + 1)
                / (s + 1)).}

            The inverse transform can be found in Miller (1978).

            Mosteller and Youtz (1961) define the transform (in our notation)
            as \eqn{f(x,s)/2}{f(x,s)/2}.
        }
        \item{Square root transform (Poisson)}{
            Let \eqn{s}{s} denote the \code{size} variable. For the Poisson
            model, it is an optional argument (and taken to be 1 if \code{size}
            is \code{NULL}). The transform is implemented as

            \deqn{f(x, s) = \sqrt{x/s} + \sqrt{(x + 1)/s}.}{f(x, s) =
                \sqrt{x/s} + \sqrt{(x + 1)/s}.}

            The original proposal of Freeman and Tukey (1950) obtains for
            \eqn{s=1}{s=1}. Allowing for \eqn{s}{s} other than one is a
            straightforward extension used in practice.
        }
        \item{A note on the inverse transformation}{
            The \code{size} argument used in \code{ft()} is stored as attribute
            \code{"size"} in the return value of \code{ft()}. This attribute is
            re-used if the inverse transform is applied.
        }
    }
}
\value{
A numeric vector with attributes \code{"trans"} and \code{"size"} (formally, an
object of class \code{ft_trans})
}
\references{
Freeman, M. F. and Tukey, J. W. (1950). Transformations related to the angular
    and the square root. \emph{Annals of Mathematical Statisits} \bold{21},
    607--611. \doi{10.1214/aoms/1177729756}

Miller, J. J. (1978). The Inverse of the Freeman--Tukey Double Arcsine
    Transformation, \emph{The American Statistician} \bold{32}, 138.
    \doi{10.1080/00031305.1978.10479283}

Mosteller, F. and Youtz, C. (1961). Tables of the Freeman--Tukey
    Transformations for the Binomial and Poisson Distributions.
    \emph{Biometrika} \bold{48}, 433--440. \doi{10.2307/2332765}
}
\author{
Tobias Schoch
}
\examples{
data("TURP")

# Square root transform with size variable ei
ft(TURP$oi, TURP$ei)
}
