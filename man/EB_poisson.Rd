\name{EBpoisson0}
\alias{EBpoisson0}
\alias{predict.negbin0}
\alias{mse.negbin0}
\title{Poisson--Gamma Empirical Bayes Estimator of Variance (With Fixed
    Variance)}
\description{
Empirical Bayes (EB) estimator of the variance in a Poisson--Gamma mixture
(with a priori expectation equal to one) and estimator/ predictor of the
area-specific rates
}
\usage{
EBpoisson0(oi, ei, interval = c(1e-5, 1e4), maxit = 1000)

\method{predict}{negbin0}(object, ...)
\method{mse}{negbin0}(object, type = c("rao", "jiang"), ...)
}
\arguments{
    \item{oi}{\code{[numeric vector]} observed counts.}
    \item{ei}{\code{[numeric vector]} expected counts.}
    \item{interval}{\code{numeric vector} of size to that defines the
        boundaries of the interval within which a root is searched.}
    \item{maxit}{\code{[integer]} maximum number of iterations to use
        (default: \code{1000}).}
    \item{type}{\code{[character]} type of jackknife MSE estimator, either
        \code{type = "rao"} or \code{type = "jiang"} (default: \code{"rao"}).}
    \item{object}{an object of class \code{sava_scv}.}
    \item{...}{additional arguments.}
}
\details{
    \describe{
        \item{Workflow}{
            The workflow is as follows: 1) estimate the parameters by
            \code{EBpoisson0()}, 2) predict the rates by \code{predict()}, and
            3) compute the mean square prediction error (MSE) using
            \code{mse()}
        }
        \item{Parameter estimation}{
            Consider the following 2-stage conditionally independent
            hierarchical Poisson--Gamma Bayesian model, where the first-stage
            model is given by

            \deqn{Y_i \mid \Lambda_i = \lambda_i \sim Pois(e_i \lambda_i)}{Y[i]
                    \mid \Lambda[i] = \lambda[i] \sim Pois(e[i] \lambda[i])}

            with \eqn{Y_i = (o_i - e_i) / e_i}{Y[i] = (o[i] - e[i]) / e[i]},
            where \eqn{o_i}{o[i]} and \eqn{e_i}{e[i]} denote, respectively, the
            observed and expected number of counts in the areas \eqn{i = 1,
                \ldots, n.}{i= 1, \ldots, n.} It is assumed that the
            \eqn{e_i}{e[i]}'s are computed by internal, indirect
            standardization (see e.g., Fleiss et al., 2003, Chapter 19). The
            a- priori distribution is

            \deqn{\Lambda_i \sim Gamma(\alpha, \alpha),}{\Lambda[i] \sim
                Gamma(\alpha, \alpha),}

            where \eqn{\alpha > 0}{\alpha >0} is the rate parameter of the
            Gamma distribution. This parametrization implies that the
            \eqn{\Lambda_i}{\Lambda_i}'s have expected value 1. The marginal
            distribution of the \eqn{Y_i}{Y[i]}'s is negative binomial in
            only the parameter \eqn{\alpha}{\alpha} (Martuzzi and Hills, 1995).

            \code{EBpoisson0()} estimates \eqn{\alpha}{\alpha} by maximum
            likelihood. The estimate of \eqn{\alpha}{\alpha} is set to
            \code{NA} if the estimate is negative or when the algorithm does
            not converge.

            By default, if any of the \eqn{o_i}{o[i]}'s is larger than 200,
            the score function of the maximum likelihood-estimator uses the
            digamma function; see \code{getOption("sava_digamma")}. The default
            value can be changed to, for example, 300 by setting
            \code{options(sava_digamma = 300)}.
        }
        \item{Prediction}{
            The EB predictor of the rates (under the above model) is computed
            by \code{predict()} based on the objected estimated by
            \code{EBpoisson0()}.
        }
        \item{Uncertainty -- Mean square prediction error estimation}{
            The mean square prediction error (MSE) is computed by the jackknife
            estimator. Two implementations are available: \code{"rao"}: Rao
            (2003, Chapter 9.5.1) or \code{"jiang"}: Jiang et al. (2002). It is
            implemented by \code{mse()}, which takes as an argument an object
            of class
            \code{sava}, i.e., output of \code{EBpoisson0()}.
        }
    }
}
\value{
\code{EBpoisson0()} returns an object of class \code{sava} (and \code{negbin0}).
The return value of, respectively, \code{predict()} and \code{mse()} are the
predicted rates and area-specific estimates of the mean square prediction
error.
}
\references{
Clayton, D. and Kaldor, J. (1987). Empirical Bayes estimates of
    age-standardized relative risks for use in disease mapping.
    \emph{Biometrics}, \bold{43}, 671--681. \doi{10.2307/2532003}

Jiang, J., Lahiri, P. and Wan, S.-M. (2002). A unified jackknife theory for
    empirical best prediction with M-estimation. \emph{The Annals of
    Statistics}, \bold{30}, 1782--1810. \doi{10.1214/aos/1043351257}

Martuzzi, M. and Hills, M. (1995). Estimating the Degree of Heterogeneity
    between Event Rates Using Likelihood. \emph{American Journal of
    Epidemiology}, \bold{141}, 369--374. \doi{10.1093/aje/141.4.369}

Rao, J. N. K. (2003). \emph{Small Area Estimation}, John Wiley and Sons:
    Hoboken (NJ).
}
\author{
Tobias Schoch
}
\seealso{
\code{\link[=SCV]{SCV()}}, \code{\link[=EBnormal0]{EBnormal0()}}
}
\examples{
data("TURP")

# estimate
est <- EBpoisson0(TURP$oi, TURP$ei)
est

# predict rates
predict(est)

# estimate mean square prediction error (type = "rao")
mse(est)
}
