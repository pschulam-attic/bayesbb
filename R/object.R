#' Create a new bayesbb
#'
#' @export
#' 
bayesbb <- function(a, b) {
    if (a <= 0) {
        stop("Hyperparameter a must be greater than 0.")
    }

    if (b <= 0) {
        stop("Hyperparameter b must be greather than 0.")
    }

    obj <- structure(list(), class = "bayesbb")
    obj$a <- a
    obj$b <- b
    obj
}

#' @export
print.bayesbb <- function(bb) {
    cat(sprintf("bayesbb(a = %.2f, b = %.2f)\n", bb$a, bb$b))
    invisible(bb)
}

#' @export
plot.bayesbb <- function(bb) {
    x <- seq(0, 1, length.out = 50)
    y <- dbeta(x, bb$a, bb$b)
    d <- data.frame(x, y)

    p <- ggplot() + geom_line(aes(x, y), data = d)
    p <- p + theme_bw()

    print(p)
    invisible(p)
}
