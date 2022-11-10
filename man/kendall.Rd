\name{kendall}
\alias{kendall}
\title{Kendall Coefficient of Concordance (Kendall's W)}
\description{
The Kendall coefficient of concordance, also known as Kendall's W, is a
rank-based measure of correlation
}
\usage{
kendall(x, na.rm = FALSE)
}
\arguments{
    \item{x}{ratings, \code{[matrix]} of dimension \code{(n, p)}, where the
        \code{p} columns contain the ratings of the \code{p} observers who
        rated the \code{n} objects.}
    \item{na.rm}{\code{[logical]} indicating whether \code{NA} values should
        be removed before the computation proceeds (default: \code{FALSE}).}
}
\details{
The Kendall coefficient of concordance can be used for assessing the agreement
among observers who rated the same objects (Kendall and Gibbons, 1990, Chapter
6). The ratings need not be whole numbers. The observer-specific ratings
(columns of matrix \code{x}) are ranked internally before computing the
coefficient.

The Kendall coefficient of concordance ranges from 0 (no agreement) to 1
(complete agreement).
}
\value{
Kendall coefficient of concordance (\code{numeric})
}
\references{
Kendall, M. and J. D. Gibbons (1990). \emph{Rank Correlation Methods}.
    New York (NY): Oxford University Press, 5th ed.
}
\author{
Tobias Schoch
}
\examples{
# Data without ties from Kendall and Gibbons (1990, Table 6.1) on
# 4 observers who rated 6 objects
x <- structure(c(5, 4, 1, 6, 3, 2, 2, 3, 1, 5, 6, 4, 4, 1, 6, 3, 2,
    5, 4, 3, 2, 5, 1, 6), dim = c(6L, 4L))
x
kendall(x)

# Data with ties from Kendall and Gibbons (1990, Table 6.2) on
# 3 observers who rated 10 objects
x <- structure(c(1, 4.5, 2, 4.5, 3, 7.5, 6, 9, 7.5, 10, 2.5, 1, 2.5,
    4.5, 4.5, 8, 9, 6.5, 10, 6.5, 2, 1, 4.5, 4.5, 4.5, 4.5, 8, 8, 8,
    10), dim = c(10L, 3L))
x
kendall(x)
}
