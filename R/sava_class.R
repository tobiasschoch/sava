# Slots for instances of class sava
# method: string
# params: parameter estimates
# model
#   center: TRUE/FALSE
#   oi: number of observed cases
#   ei: number of expected cases
# converged: TRUE/FALSE
# optim (only available if converged == TRUE)
#   niter: number of iterations until covergence was declared
#   tol: numeric tolerance
# call

# empty instance of class sava
.empty_instance <- function(method = NA, params = NA_real_, model = NA,
    call = NA, class = "sava")
{
    structure(list(method = method, params = params, model = model,
        converged = FALSE, call = call), class = class)
}

# S3 print method
print.sava <- function(x, digits = max(1L, getOption("digits") - 2L), ...)
{
    if (!x$converged) {
        cat("\nAlgorithm did not converge\n\n")
    } else {
        cat(paste0("\n", x$method, "\nParameter estimate(s):\n"))
        print(unlist(lapply(x$params, round, digits = digits)))
        if (!is.null(x$optim))
            cat(paste0("\nAlgorithm converged in ", x$optim$niter,
                " iterations\n"))
    }
}
# S3 method to extract parameter estimates
params <- function(object, ...)
{
    UseMethod("params")
}

params.sava <- function(object, ...)
{
    object$params
}

# MSE estimation
mse <- function(object, ...)
{
    UseMethod("mse")
}

# Jackknife variance estimator
jackknife <- function(object, ...)
{
    if (!inherits(object, "sava"))
        stop(call. = FALSE)

    base <- object$params[[1]]
    if (inherits(object, "negbin0"))
        base <- 1 / base

    oi <- object$model$oi; ei <- object$model$ei
    n <- object$model$n
    call <- object$call
    call[[2]] <- substitute(oi[-i])
    call[[3]] <- substitute(ei[-i])

    res <- rep(0, n)
    for (i in 1:n) {
        tmp <- eval(call)
        est <- tmp$params[[1]]
        if (inherits(object, "negbin0"))
            est <- 1 / est
        res[i] <- (est - base)^2
    }
    sum(res) / (n - 1)
}

# extremal quotient
EQ <- function(object, q = 0.2)
{
    stopifnot(is.numeric(q), length(q) == 1, q > 0, q < 0.5)
    if (!inherits(object, c("negbin0", "genjs0")))
        stop("EQ cannot be computed for this object\n", call. = FALSE)
    # predicted values
    yhat <- predict(object)
    if (length(yhat) > 5) {
        tmp <- quantile(yhat, probs = c(q, 1 - q))
        unname(tmp[2] / tmp[1])
    } else {
        NA
    }
}
# quntile ratio (wrapper function for EQ)
QR <- function(object)
{
    EQ(object, q = 0.2)
}

# extremal quotient
QSR <- function(object, q = 0.2)
{
    stopifnot(is.numeric(q), length(q) == 1, q > 0, q < 0.5)
    if (!inherits(object, c("negbin0", "genjs0")))
        stop("EQ cannot be used for this object\n", call. = FALSE)

    yhat <- predict(object)
    tmp <- quantile(yhat, probs = c(q, 1 - q))
    unname(mean(yhat[yhat >= tmp[2]]) / mean(yhat[yhat <= tmp[1]]))
}
