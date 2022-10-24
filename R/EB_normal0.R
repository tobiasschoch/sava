EBnormal0 <- function(oi, ei, maxit = 100, tol = 1e-5)
{
    yi <- (oi - ei) / ei
    di <- oi / ei^2

    stopifnot(all(di >= 0), length(yi) == length(di))

    at <- complete.cases(yi, di)
    if (sum(at) != length(at))
        stop("Some values are missing or not a number\n")

   # skeleton of return value
    res <- .empty_instance(
        method =
            "Generalized James-Stein MLE estimator (location fixed at origin)",
        model = list(oi = oi, ei = ei, yi = yi, di = di, n = length(yi)),
        call = match.call(), class = c("sava", "genjs0"))

    # return NA if expected value is zero
    if (any(ei == 0)) {
        warning("Some of the expected values are zero\n", call. = FALSE)
        return(res)
    }

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

    # if converged (otherwise params = NA, converged = FALSE, optim = NULL)
    if (converged) {
        res$params <- c(A = A)
        res$converged <- TRUE
        res$optim <- list(niter = niter, tol = tol, negflag = negflag)
    }
    res
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
    yi <- object$model$yi; di <- object$model$di

    # shrinkage factor
    Bi <- di / (object$params[[1]] + di)

    # curvature correction of Morris (1983, JASA)
    Bi <- (object$model$n - 3) / (object$model$n  - 2) * Bi

    # EB rule
    delta <- (1 - Bi) * yi + Bi

    # EB rate
    delta + 1
}

# S3 mse method for genJS
mse.genjs0 <- function(object, method = "analytic", ...)
{
    switch(match.arg(method, c("analytic", "jackknife")),
        "analytic" = .mse_analytic(object),
        "jackknife" = .mse_jackknife(object))
}

# mse analytic approximation (Prasad & Rao, 1990, JASA)
.mse_analytic <- function(object)
{
    di <- object$model$di
    A <- object$params[[1]]
    Bi <- di / (A + di)
    # first component
    g1 <- A * Bi
    # second component (zero because location is not estimated)
    g2 <- 0
    # third component (the same for MLE and REML; see Datta & Lahiri, 2000,
    # Statistica Sinica)
    g3 <- 2 * Bi^2 / ((A + di) * sum(1 / (A + di)^2))
    # in total
    g1 + g2 + g3
}

# Jackknife mse estimator of Jiang, Lahiri & Wang  (2002, Ann. Stat.)
.mse_jackknife <- function(object)
{
    yi <- object$model$yi; di <- object$model$di
    n <- object$model$n
    delta_base <- predict(object)
    # g1 component of mse
    g1_base <- object$params[[1]] * di / (object$params[[1]] + di)

    oi <- object$model$oi; ei <- object$model$ei
    call <- object$call
    call[[2]] <- substitute(oi[-i])
    call[[3]] <- substitute(ei[-i])

    m1 <- rep(0, n); m2 <- rep(0, n)
    for (i in 1:n) {
        tmp <- eval(call)
        tmp$model <- list(yi = yi, di = di, n = length(yi))
        A <- tmp$params[[1]]
        # jackknife estimator of rule delta
        m1 <- m1 + (A * di / (A + di) - g1_base)^2
        m2 <- m2 + (predict(tmp) - delta_base)^2
    }
    m1 <- m1 * (n - 1) / n
    m2 <- m2 * (n - 1) / n
    # mse estimator
    g1_base - m1 + m2
}
