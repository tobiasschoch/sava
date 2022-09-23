# Freeman and Tukey (1950). Transformations Related to the Angular and the
# Square Root, Annals of Mathetmatical Staistics 21, 607-611
ft <- function(x, size = NULL, type = c("root", "arcsin"))
{
    stopifnot(is.numeric(x), all(x >= 0))
    type <- match.arg(type)
    if (is.null(size)) {
        if (type == "arcsin")
            stop("Argument 'size' must be specified for type 'arcsin'\n")
        else
            size <- rep(1, length(x))
    }
    stopifnot(length(size) == length(x), all(size >= 0))

    res <- switch(type,
        "root"   = sqrt(x / size) + sqrt((x + 1) / size),
	    "arcsin" = {
            denom <- size + 1
      	    asin(sqrt(x / denom)) + asin(sqrt((x + 1) / denom))
        })
    attributes(res) <- list(transform = type, size = size,
        class = c("ft_trans", "numeric"))
    res
}
# Miller (1978). The Inverse of the Freeman-Tukey Double Arcsine
# Transformation, The American Statistician 32, 138
ft_inv <- function(x)
{
    stopifnot(is.numeric(x))
    size <- attr(x, "size")
    type <- attr(x, "transform")
    if (is.null(type))
        stop("Inverse transform is not applicable\n")

    res <- switch(type,
	    "root"   = {
            nx2 <- size * x^2
            (-1 + nx2)^2 / (4 * nx2)
        },
   	    "arcsin" = {
            sx <- sin(x); cx <- cos(x)
            size * (1 - sign(cx) * sqrt(1 - (sx + (sx - 1 / sx) / size)^2)) / 2
        })
    attributes(res) <- NULL
    res
}
# S3 print method
print.ft_trans <- function(x, ...)
{
    attributes(x) <- NULL
    print(x, ...)
}
