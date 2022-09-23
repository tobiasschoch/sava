ranks <- function(object)
{
    stats::cor(order(object$model$oi / object$model$ei),
        order(predict(object)), method = "kendal")
}

plot.sava <- function(x, ...){
    rate <- x$model$oi / x$model$ei
    plot(rate, predict(x), xlab = "rate", ylab = x$call[[1]])
    abline(0, 1)
}


