bayesbb_has_evidence <- function(bb) {
    !is.null(bb$x)
}

bayesbb_append_evidence <- function(bb, x) {
    bb$x <- c(bb$x, x)
    bb
}
