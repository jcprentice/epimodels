#' Get T_max
#'
#' Find `tmax` for each trial, after possibly censoring the last proportion of
#' the population, given by `censor`, and rounded up to the next `time_step`
#' value.
#'
#' @param popn A Population with event times
#' @param params A list of parameters
#'
#' @returns The maximum model time
#' @export

get_tmax <- function(popn, params) {
    censor <- params$censor
    time_step <- params$time_step
    sim_new_data <- params$sim_new_data

    if (sim_new_data == "no") {
        return(c(t1 = 104, t2 = 160))
    }

    x1 <- popn[sdp == "progeny", .(id, trial, Tdeath)]

    x1[, Tdeath := fifelse(is.na(Tdeath),
                           max(Tdeath, na.rm = TRUE),
                           Tdeath),
       trial]

    x2 <- x1[, quantile(Tdeath, censor, na.rm = TRUE), trial]
    tmax <- setNames(x2$V1, str_c("t", x2$trial))

    # Want the nearest time step if time is measured in discrete intervals
    if (time_step > 0) {
        tmax <- ceiling(tmax / time_step) * time_step
    }

    tmax
}
