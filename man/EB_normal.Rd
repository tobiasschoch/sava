\name{EBnormal0}
\alias{EBnormal0}
\alias{predict.genjs0}
\alias{mse.genjs0}
\title{Normal--Normal Empirical Bayes Estimator (With Fixed Location)}
\description{
Empirical Bayes (EB) estimator of the variance in a Normal--Normal mixture
(with a priori location fixed at the origin) and the generalized James--Stein
estimator/ predictor of the area-specific rates
}
\usage{
EBnormal0(oi, ei, maxit = 100, tol = 1e-5)

\method{predict}{genjs0}(object, ...)
\method{mse}{genjs0}(object, method = "analytic", ...)
}
\arguments{
    \item{oi}{\code{[numeric vector]} area-specific observed counts.}
    \item{ei}{\code{[numeric vector]} area-specific expected counts.}
    \item{maxit}{\code{[integer]} maximum number of iterations to use
        (default: \code{100}).}
    \item{tol}{\code{[double]} numerical tolerance criterion to stop the
        iterations (default: \code{1e-05}).}
    \item{object}{an object of class \code{sava_scv}.}
    \item{method}{\code{[character]} type of mse estimator, \code{"analytic"}
        or \code{"jackknife"} (default: \code{"analytic"}).}
    \item{...}{additional arguments.}
}
\details{
    \describe{
        \item{Workflow}{
            The workflow is as follows: 1) estimate the parameters by
            \code{EBnormal0()}, 2) predict the rates by \code{predict()}, and
            3) compute the mean square prediction error using \code{mse()}
        }
        \item{Parameter estimation}{
            Consider the following 2-stage conditionally independent
            hierarchical Normal--Normal Bayesian model, where the first-stage
            model is given by

            \deqn{Y_i \mid \Theta_i = \theta_i \sim N(\theta_i, D_i)}{Y[i] \mid
                \Theta[i] = \theta[i] \sim  N(\theta[i], D[i])}

            with \eqn{Y_i = (o_i - e_i) / e_i}{Y[i] = (o[i] - e[i]) / e[i]},
            where \eqn{o_i}{o[i]} and \eqn{e_i}{e[i]} denote, respectively, the
            observed and expected number of counts in the areas \eqn{i = 1,
                \ldots, n}{i= 1, \ldots, n}. It is assumed that the
            \eqn{e_i}{e[i]}'s are computed by internal, indirect
            standardization (see e.g., Fleiss et al., 2003, Chapter 19). The
            variances \eqn{D_i}{D[i]}'s are known quantities, which are
            defined as \eqn{D_i = o_i / e_i^2}{D[i] = o[i] / e[i]^2} (Shwartz
            et al., 1994). The a priori distribution is

            \deqn{\Theta_i \sim N(0, A)}{\Theta_i \sim N(0, A)}

            where the variance \eqn{A}{A} is unknown; see also Efron and Morris
            (1975).

            \code{EBnormal0()} estimates \eqn{A}{A} by maximum likelihood. The
            estimate of \eqn{A}{A} is set to \code{NA} if the estimate is
            negative or when the algorithm does not converge.

            \emph{Note.} The a priori location is zero. Hence, the EB
            prediction rule shrinks towards the origin. This is reflected in
            the name of \code{EBnormal0} by the suffix \code{0}.
        }
        \item{Prediction}{
            The EB predictor of the rates (which is a generalized
            James--Stein estimator about the origin, see Efron and Morris,
            1975) is computed by \code{predict()} based on the objected
            estimated by \code{EBnormal0()}. The implemented predictor
            applies the bias correction proposed in Morris (1983).
        }
        \item{Uncertainty -- Mean square prediction error estimation}{The mean
            square prediction error can be computed by 1) the Prasad and Rao
            (1990) analytic approximation (\code{method = "analytic"}) or 2)
            the jackknife estimator of Jiang et al. (2002), see \code{method =
                "jackknife"}. It is implemented by \code{mse()}, which takes as
            an argument an object of class \code{sava}, i.e., output of
            \code{EBnormal0()}.
        }
    }
}
\value{
\code{EBnormal0()} returns an object of class \code{sava} (and \code{genjs0}).
The return value of, respectively, \code{predict()} and \code{mse()} are the
predicted rates and area-specific estimates of the mean square prediction
error.
}
\references{
Efron, B. und Morris, C. (1975). Data Analysis Using Stein’s Estimator and its
    Generalizations. \emph{Journal of the American Statistical Association}
    \bold{70}, 311--19. \doi{10.1080/01621459.1975.10479864}

Fleiss, J. L., Levin, B. und Paik, M. C. (2003). \emph{Statistical Methods for
    Rates and Proportions}, Hoboken (NJ): John Wiley and Sons, 3rd ed.

Jiang, J., Lahiri, P. und Wan, S.-M. (2002). A unified jackknife theory for
    empirical best prediction with M-estimation. \emph{The Annals of
    Statistics} \bold{30}, 1782--1810. \doi{10.1214/aos/1043351257}

Morris, C. N. (1983). Parametric Empirical Bayes Inference: Theory and
    Applications. \emph{Journal of the American Statistical Association}
    \bold{78}, 47--55. \doi{10.1080/01621459.1983.10477920}

Prasad, N. G. N. and Rao, J. N. K. (1990). The Estimation of the Mean Squared
    Error of Small-Area Estimators. \emph{Journal of the American Statistical
    Association} \bold{85}, 163-171. \doi{10. 2307/2289539}

Shwartz, M., Ash, A. S., Anderson, J., Iezzoni, L. I., Payne, S. M. C. und
    Restuccia, J. D. (1994). Small Area Variations in Hospitalization Rates:
    How Much You See Depends on How You Look. \emph{Medical Care} \bold{32},
    189--201.  \doi{10.1097/00005650-199403000-00001}
}
\author{
Tobias Schoch
}
\seealso{
\code{\link[=SCV]{SCV()}}, \code{\link[=EBpoisson0]{EBpoisson0()}}
}
\examples{
data("TURP")

# estimate
est <- EBnormal0(TURP$oi, TURP$ei)
est

# predict rates
predict(est)

# estimates of the mean square prediction error
mse(est)
}
