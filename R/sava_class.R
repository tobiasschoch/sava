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
