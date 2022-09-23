# Systematic component of variation (McPherson et al., 1982)
SCV <- function(oi, ei, approx = FALSE)
{
    at <- complete.cases(oi, ei)
    if (sum(at) != length(at))
        stop("Some values are missing or not a number\n")

    stopifnot(all(ei > 0))

    if (approx) {   # specification in Diehr et al. (1990)
        stopifnot(all(oi >= 0))
        yi <- (oi - ei) / ei

    } else {        # original specification of McPherson et al. (1982)
        stopifnot(all(oi > 0))
        yi <- log(oi) - log(ei)
    }

    converged <- TRUE
    scv <- mean(yi^2 - 1 / ei)
    if (scv < 0) {  # negative value: SCV is not well defined
        scv <- NA
        converged <- FALSE
    }

    structure(list(
        method = paste0("Systematic component of variation", if (approx)
            " (definition of Diehr et al., 1990)"),
        params = c(scv = scv),
        model = list(oi = oi, ei = ei, n = length(ei), approx = approx),
        converged = converged,
        call = match.call()),
        class = c("sava", "sava_scv"))
}

# S3 prediction method
predict.sava_scv <- function(object, ...)
{
    object$model$oi / object$model$ei
}
