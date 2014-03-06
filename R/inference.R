#' Condition on data
#'
#' Observe data and condition on it to obtain posterior probabilities
#' over models.
#'
#' @export
#' 
observe <- function(...) UseMethod("observe")

#' @export
observe.bayesbb <- function(bb, x) {
    x <- as.logical(x)

    if (!bayesbb_has_evidence(bb)) {
        bb$prior <- bb
    }

    bb <- bayesbb_append_evidence(bb, x)

    a <- bb$a + sum(x)
    b <- bb$b + length(x) - sum(x)

    bb$a <- a
    bb$b <- b
    bb
}

#' Sample models
#'
#' @export
#' 
samples <- function(...) UseMethod("samples")

#' @export
samples.bayesbb <- function(bb, n) {
    a <- bb$a
    b <- bb$b
    x <- rbeta(n, a, b)
    x
}

#' @export
mean.bayesbb <- function(bb, moment = 1) {

    moment1 <- function(a, b) {
        a / (a + b)
    }

    moment2 <- function(a, b) {
        (a * b) / (a + b)^2 / (a + b + 1)
    }

    switch(moment,
           moment1(bb$a, bb$b),
           moment2(bb$a, bb$b))
}

