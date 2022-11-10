# Kendall coefficient of concordance W (without ties)
kendall <- function(x, na.rm = FALSE)
{
    if (!is.matrix(x))
        x <- as.matrix(x)
    cc <- complete.cases(x)
    if (sum(cc) != NROW(x)) {
        if (na.rm)
            x <- x[cc, ]
        else
            return(NA)
    }
    # number of observers
    m <- NCOL(x)
    if (m <= 1) {
        warning("Data contains only one rating\n", call. = FALSE)
        return(1)
    }
    # number of ratings
    n <- NROW(x)
    # compute ranks
    r <- apply(x, 2, rank)
    # ties
    ties <- apply(r, 2, function(u) {
        tu <- table(u)
        ts <- tu[tu > 1]
        sum(ts^3 - ts)
    })
    # coefficient of concordance
    S <- var(apply(r, 1, sum)) * (n - 1)
    maxS <- (m^2 * (n^3 - n) - m * sum(ties)) / 12
    S / maxS
}
