.onLoad <- function(...)
{
    if (is.null(getOption("sava_digamma")))
        options(sava_digamma = 200)
}
