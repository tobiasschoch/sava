EBnormal0 <- function(oi, ei, maxit = 100, tol = 1e-5)
{
    yi <- (oi - ei) / ei; di <- oi / ei^2
    stopifnot(all(di > 0), length(yi) == length(di))

    at <- complete.cases(yi, di)
    if (sum(at) != length(at))
        stop("Some values are missing or not a number\n")

    # initialization: ML estimator of variance (location kept fixed at zero)
    # Fisher scoring
    A <- 1; w <- 1 / (A + di)
    negflag <- FALSE; niter <- 0; converged <- FALSE
    while (niter <= maxit) {
        niter <- niter + 1
        Anew <- A + .normal0_scoring(w, yi)
        # if Anew is negative, we put Anew <- 0 (and set the negflag), the
        # second time we encounter a negative value, we terminate
        if (Anew < 0) {
            Anew <- 0
            if (negflag)
                break
            negflag <- TRUE
        }
        if (abs(Anew - A) < tol) {
            converged <- TRUE
            break
        } else {
            A <- Anew
            w <- 1 / (A + di)
        }
    }

    structure(list(
        method =
            "Generalized James-Stein MLE estimator (location fixed at origin)",
        params = c(A = A),
        model = list(oi = oi, ei = ei, n = length(yi)),
        converged = converged,
        optim = list(niter = niter, tol = tol, negflag = negflag),
        call = match.call()),
        class = c("sava", "genjs0"))
}

# internal function used in Fisher scoring algorithm
.normal0_scoring <- function(w, ri)
{
    J <- sum(w^2)
    S <- -sum(w) + sum((w * ri)^2)
    S / J
}

# S3 prediction method
predict.genjs0 <- function(object, ...)
{
    oi <- object$model$oi; ei <- object$model$ei
    yi <- (oi - ei) / ei; di <- oi / ei^2

    # shrinkage factor
    Bi <- di / (object$params[[1]] + di)

    # curvature correction of Morris (1983, JASA)
    Bi <- (object$model$n - 3) / (object$model$n  - 2) * Bi

    # EB rule
    delta <- (1 - Bi) * yi + Bi

    # EB rate
    delta + 1
}


