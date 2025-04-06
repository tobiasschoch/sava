# Poisson-Gamma model restricted to variance of Gamma prior equal to 1
EBpoisson0 <- function(oi, ei, interval = c(1e-5, 1e4), maxit = 1000)
{
    stopifnot(all(ei >= 0), all(oi >= 0), length(ei) == length(oi))

    at <- complete.cases(oi, ei)
    if (sum(at) != length(at))
        stop("Some values are missing or not a number\n")

    # skeleton of return value
    res <- .empty_instance(
        method = "NegBin MLE estimator (one-parameter)",
        model = list(oi = oi, ei = ei, n = length(ei)),
        call = match.call(), class = c("sava", "negbin0"))

   # return NA if expected value is zero
    if (any(ei == 0)) {
        warning("Some of the expected values are zero\n", call. = FALSE)
        return(res)
    }

    # check sign change over interval
    use_digamma <- getOption("sava_digamma")
    if (is.null(use_digamma))
        stop("Option 'sava_digamma' is not defined\n", call. = FALSE)

    if (.mle_negbin0(interval[2], oi, ei, use_digamma) > 0) {
        warning("Maximum-likelihood estimator is undefined\n", call. = FALSE)
        return(res)
    }

    # compute root
    tmp <- uniroot(.mle_negbin0, interval = interval, oi = oi, ei = ei,
        use_digamma = use_digamma, maxiter = maxit)

    if (!is.na(tmp$root)) {
        res$params <- c(alpha = tmp$root)
        res$converged <- TRUE
        res$optim <- list(niter = tmp$iter, interval = interval, maxit = maxit)
    }
    res
}

# MLE of one-parameter negative binomial (score function)
.mle_negbin0 <- function(alpha, oi, ei, use_digamma)
{
    n <- length(oi)
    mx <- max(oi)
    term_1 <- if (mx > use_digamma) {
        sum(digamma(alpha + oi)) - n * digamma(alpha)
    } else {
        indx <- seq_len(mx) - 1
        cs <- cumsum(1 / (indx + alpha))
        cs <- c(0, cs)
        sum(cs[oi + 1])
    }
    term_1 + n * (1 + log(alpha)) - sum((oi + alpha) / (ei + alpha)) -
        sum(log(ei + alpha))
}

# S3 prediction method
predict.negbin0 <- function(object, ...)
{
    alpha <- params(object)
    (object$model$oi + alpha) / (object$model$ei + alpha)
}

# S3 jackknife estimator mse
mse.negbin0 <-function(object, type = c("rao", "jiang"), ...)
{
    alpha <- params(object)
    # Jackknife: shape
    oi <- object$model$oi; ei <- object$model$ei; n <- object$model$n
    alpha_delete_one <- numeric(n)
    for (i in 1:n)
        alpha_delete_one[i] <- params(EBpoisson0(oi[-i], ei[-i], interval =
            object$optim$interval, maxit = object$optim$maxit))
    # Estimate of M1 (using posterior variance g; see Rao, 2003, p. 199)
    switch(match.arg(type, c("rao", "jiang")),
        "rao" = {
            g <- (oi + alpha) / (ei + alpha)^2
            M1 <- g - sapply(as.list(alpha_delete_one), function(x){
                sum((oi + x) / (ei + x)^2 - g)}) * (n - 1) / n
        },
        "jiang" = {
            g <- 1 / (ei + alpha)
            M1 <- g - sapply(as.list(alpha_delete_one), function(x){
                sum(1 / (ei + x) - g)}) * (n - 1) / n
        })
    # truncate at zero (treat negative values)
    M1 <- pmax(0, M1)
    # Estimate of M2
    delta <- predict(object)
    M2 <- sapply(as.list(alpha_delete_one), function(x){
        sum(((oi + x) / (ei + x) - delta)^2)}) * (n - 1) / n
    M1 + M2
}
