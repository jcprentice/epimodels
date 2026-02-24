#' Check Likelihoods
#'
#' Plot the likelihoods along MCMC chains
#'
#' @param dataset A dataset string
#' @param scen A scenario number
#'
#' @returns A plot of Likelihood values
#' @export

check_L <- function(dataset = "fb-final", scen = 1) {
    if (FALSE) {
        dataset <- "fb-test"; scen <- 3
    }

    files <- list.files(str_glue("datasets/{dataset}/data/",
                                 "scen-{scen}-1-out/output-inf"),
                        pattern = "param_", full.names = TRUE) |>
        str_subset("combine", negate = TRUE) |>
        str_sort(numeric = TRUE)

    if (length(files) == 0) {
        message("- No param_ files found!")
        return(NULL)
    }

    x <- map(files, fread) |>
        rbindlist(idcol = "chain")
    x[, str_subset(names(x), "chain|State|^L\\^", negate = TRUE) := NULL]
    x[, State := State / State[2]]

    # Thin down to at most 1e4 samples
    x1 <- x[seq(1, .N, length.out = 1e4) |> ceiling() |> unique()]
    x1[, `:=`(State = seq(.N),
              chain = as.factor(chain))]

    Lcols <- c("L^markov", "L^ie", "L^dist")
    x1[, names(.SD) := map(.SD, as.numeric), .SDcols = Lcols]
    x2 <- melt(x1, measure.vars = Lcols)

    plt <- ggplot(x2) +
        geom_line(aes(x = State, y = value, colour = chain)) +
        facet_wrap(. ~ variable, scales = "free_y", ncol = 1)

    plt
}
