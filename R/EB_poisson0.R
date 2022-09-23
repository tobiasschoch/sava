# Poisson-Gamma model restricted to variance of Gamma prior equal to 1
EBpoisson0 <- function(oi, ei, interval = c(1e-5, 1e4), maxit = 1000)
{
    stopifnot(all(ei > 0), all(oi >= 0), length(ei) == length(oi))
    at <- complete.cases(oi, ei)
    if (sum(at) != length(at))
        stop("Some values are missing or not a number\n")

    tmp <- uniroot(.mle_negbin0, interval = interval, oi = oi, ei = ei,
        maxiter = maxit)
    structure(list(
        method = "NegBin MLE estimator (one-parameter)",
        params = c(alpha = tmp$root),
        model = list(oi = oi, ei = ei, n = length(oi)),
        converged = ifelse(is.na(tmp$root), FALSE, TRUE),
        optim = list(niter = tmp$iter, interval = interval),
        call = match.call()),
        class = c("sava", "negbin0"))
}

# MLE of one-parameter negative binomial
.mle_negbin0 <- function(alpha, oi, ei)
{
    n <- length(oi)
    indx <- seq_len(max(oi)) - 1
    cs <- cumsum(1 / (indx + alpha))
    cs <- c(0, cs)
    sum(cs[oi + 1]) + n * (1 + log(alpha)) - sum((oi + alpha) / (ei + alpha)) -
        sum(log(ei + alpha))
}

# S3 prediction method
predict.negbin0 <- function(object, ...)
{
    alpha <- object$params[[1]]
    (object$model$oi + alpha) / (object$model$ei + alpha)
}
