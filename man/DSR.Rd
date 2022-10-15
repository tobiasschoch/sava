\name{DSR}
\alias{DSR}
\title{Direct Standardized Rates and Confidence Intervals}
\description{
Direct standardized rates are computed given the observed count data and the
weights (by stratum). Confidence interval estimation follows the Fay and Feuer
(1997).
}
\usage{
DSR(oi, wi, conf.level = 0.95, scale = 1, simple = FALSE, empty = TRUE)
}
\arguments{
    \item{oi}{\code{[numeric vector]} observed counts (by stratum).}
    \item{wi}{\code{[numeric vector]} weights (by stratum).}
    \item{conf.level}{\code{[numeric]} confidence level of the interval
        (default: \code{0.95}).}
    \item{scale}{\code{[numeric]} scale factor, choose e.g., \code{scale =
        1000} to compute rates per 1,000 individuals (default: \code{1}).}
    \item{simple}{\code{[logical]} if \code{TRUE}, the return value is the
        confidence interval, i.e., a vector of size 2; if \code{FALSE}, a list
        is returned (default: \code{FALSE}).}
    \item{empty}{\code{[logical]} if \code{TRUE}, the confidence interval
        for vectors with not observations is \code{c(0, Inf)}; if \code{FALSE},
        the confidence interval is \code{c(NA, NA)} (default: \code{TRUE}.}
}
\details{
The direct standardized rate (DSR) is computed as the weighted sum of the
\code{oi} with weights \code{wi}, which is maximum-likelihood estimator. The
confidence intervals are computed under the assumption that the rate can
be modeled as weighted sum of independent Poisson random variables. The
construction has been proposed by Fay and Feuer (1997). The simulation study of
Ng et al. (2008) showed that this construction leads to conservative
confidence intervals.

The validity of the confidence intervals relies on the Poisson assumption. If
the distribution of the count data shows significant overdispersion, the
computed confidence intervals tend to be too short.
}
\value{
If \code{simple = FALSE}, the return value is the confidence interval (a
numeric vector of size 2), otherwise a list with the following slots is
returned
    \itemize{
        \item{\code{DSR}}{direct standardized rate}
        \item{\code{conf.int}}{confidence interval, vector} (with attributes)
        \item{\code{scale}}{scaling factor}
        \item{\code{method}}{description of method}
    }
}
\references{
Fay, M. P. and Feuer, E. J. (1997). Confidence intervals for directly
    standardized rates: A method based on the gamma distribution.
    \emph{Statistics in Medicine} \bold{16}, 791--801.
    \doi{10.1002/(SICI)1097-0258(19970415)16:7<791::AID-SIM500>3.0.CO;2-\\\%23}

Ng, H. K. T., Filardo, G. und Zheng, G. (2008). Confidence interval estimating
    procedures for standardized incidence rates.
    \emph{Computational Statistics and Data Analysis} \bold{52}, 3501--3516.
    \doi{10.1016/j.csda.2007.11.004}
}
\author{
Tobias Schoch
}
\examples{
# Observed count data (by age group)
oi <- c(0 ,8 , 63, 112, 262, 295)

# Number of individuals (by age group) in the study population
ni <- c(327, 30666, 123419, 149919, 104088, 34392)

# Shares of the age groups in the standard population (e.g., European Standard
# Population). The share sum up to 1.
si <- c(0.1133, 0.3296, 0.2784, 0.1728, 0.0842, 0.0217)

# Weights (by age group)
wi <- si / ni

# Direct standardized rate and 95% confidence interval (default)
DSR(oi, wi)
}
