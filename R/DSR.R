# direct standardized rates
DSR <- function(oi, wi, conf.level = 0.95, scale = 1, simple = FALSE,
    empty = TRUE)
{
    stopifnot(all(oi >= 0), all(wi >= 0), all(is.finite(wi)), conf.level > 0,
        conf.level < 1, scale > 0)
    if (length(oi) != length(wi))
        stop("Vectors oi and wi must be of the same length\n", call. = FALSE)

    # mle of rate and variance estimate
    r <- sum(wi * oi)
    v <- sum(wi^2 * oi)

    # CI Fay and Feuer (1997, Stat in Med)
    if (r > .Machine$double.eps) {
        max_w <- max(wi)
        lo <- v / (2 * r) * qchisq(0.5 - conf.level / 2, 2 * r^2 / v)
        hi <- (v + max_w^2) / (2 * (r + max_w)) * qchisq(0.5 + conf.level / 2,
            2 * (r + max_w)^2 / (v + max_w^2))
    } else if (empty) {
        lo <- 0
        hi <- Inf
    } else {
        lo <- hi <- NA
    }
    ci <- scale * c(lo, hi)
    if (simple) {
        ci
    } else {
        attr(ci, "conf.level") <- conf.level
        list(DSR = scale * r, conf.int = ci, scale = scale,
             method = "Weighted Poisson CI, Fay and Feuer (1997, Stat Med)")
    }
}
