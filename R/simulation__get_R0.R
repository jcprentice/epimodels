#' Get R0
#'
#' Calculate R0 from the output of a simulated epidemic using the ratio of Gen 1
#' to Gen 2 infectives.
#'
#' @param popn A Population with infection generations.
#'
#' @returns An estimated R0
#' @export

get_R0 <- function(popn) {
    x <- table(popn$generation)

    R0 <- if (length(x) > 1) x[[2]] / x[[1]] else 0

    message("R0 estimate: ", signif(R0, 3))

    R0
}

