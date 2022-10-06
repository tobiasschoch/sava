# Systematic component of variation (McPherson et al., 1982)
SCV <- function(oi, ei, approx = FALSE)
{
    at <- complete.cases(oi, ei)
    if (sum(at) != length(at))
        stop("Some values are missing or not a number\n")

    stopifnot(all(ei > 0), all(oi >= 0))

    # skeleton of return value
    res <- .empty_instance(
        method = paste0("Systematic component of variation", if (approx)
            " (definition of Diehr et al., 1990)"), model =
        list(oi = oi, ei = ei, n = length(ei), approx = approx),
        call = match.call(),
        class = c("sava", "sava_scv"))

    yi <- if (approx)   # specification in Diehr et al. (1990)
        (oi - ei) / ei
    else                # original specification of McPherson et al. (1982)
        log(oi) - log(ei)

    scv <- mean(yi^2 - 1 / ei)
    names(scv) <- "scv"

    if (is.infinite(scv)) {
        res$params <- NA_real_
        res$converged <- FALSE
    } else if (scv < 0) {
        res$params <- 0
        res$converged <- FALSE
    } else {
        res$converged <- TRUE
        res$params <- scv
    }
    res
}

# S3 prediction method
predict.sava_scv <- function(object, ...)
{
    object$model$oi / object$model$ei
}

# Marshall (1991) estimator
marshall <- function(oi, ei)
{
    at <- complete.cases(oi, ei)
    if (sum(at) != length(at))
        stop("Some values are missing or not a number\n")

    stopifnot(all(ei > 0), all(oi >= 0))

    # skeleton of return value
    n <- length(ei)
    res <- .empty_instance(
        method = "Marshall estimator", model = list(oi = oi, ei = ei, n = n),
        call = match.call(), class = c("sava", "sava_scv"))

    tau2 <- (sum((oi - ei)^2 / ei) - n) / sum(ei)
    names(tau2) <- "tau2"

    if (tau2 < 0) {
        res$params <- 0
        res$converged <- FALSE
    } else {
        res$params <- tau2
        res$converged <- TRUE
    }
    res
}
