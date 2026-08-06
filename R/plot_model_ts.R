#' Plot Model time series
#'
#' @param popn A population with epidemic event times
#' @param params A list of parameters
#'
#' @returns A plot of the epidemic
#' @export

plot_model <- function(popn, params) {
    plt <- switch(params$model_type,
                  "SI" = plot_SI(popn, params), # not yet implemented
                  "SIS" = plot_SIS(popn, params),
                  "SIR" = plot_SIR(popn, params),
                  "SEIR" = plot_SEIR(popn, params),
                  "SIDR" = plot_SIDR(popn, params),
                  "SEIDR" = plot_SEIDR(popn, params))

    if (params$show_plots) {
        print(plt)
    }
    plt
}


plot_SxxDR <- function(popn, params) {
    message("Plotting SxxDR model")

    N <- popn[sdp == "progeny", .N]
    tmax <- params$tmax

    events <- make_time_series_seidr(popn, params)
    events[, S := S + E + I]
    events[, c("E", "I", "ID") := NULL]

    if (params$sim_new_data == "no") {
        events <- events[time != max(time)]
    }

    ggplot(events, aes(x = time)) +
        geom_line(aes(y = S / N, colour = "Susceptible?"), linewidth = 1.2) +
        geom_line(aes(y = D / N, colour = "Detectable"),   linewidth = 0.6) +
        geom_line(aes(y = R / N, colour = "Removed"),      linewidth = 1.2) +
        scale_colour_manual("Compartments",
                            breaks = c("Susceptible?", "Detectable", "Removed"),
                            values = c("blue", "red", "green")) +
        labs(title = "SxxDR", x = "Time (days)", y = "Proportion") +
        coord_cartesian(xlim = c(0, min(tmax, max(events$time), na.rm = TRUE)),
                        ylim = c(0, 1)) +
        theme_bw() +
        theme(legend.position = "bottom")
}

plot_SEIDR <- function(popn, params) {
    message("Plotting SEIDR model")

    N <- popn[sdp == "progeny", .N]
    tmax <- max(params$tmax)

    events <- make_time_series_seidr(popn, params)

    ggplot(events) +
        aes(x = time,
            y = value / N,
            colour = variable) +
        geom_line(linewidth = 1.2) +
        scale_colour_manual("Compartments",
                            breaks = c("S", "E", "I", "D", "ID", "R"),
                            labels = c("Susceptible (S)", "Exposed (E)",
                                       "Undetectable (I)", "Detectable (D)",
                                       "Infectious (I+D)", "Removed (R)"),
                            # values = c("blue", "pink", "purple", "darkgreen", "green", "red")) +
                            values = c(viridisLite::viridis(5), "red")) +
        coord_cartesian(xlim = c(0, min(tmax, max(events$time), na.rm = TRUE)),
                        ylim = c(0, 1)) +
        labs(x = "Time (days)",
             y = "Proportion",
             title = "SEIDR model") +
        theme_bw()
}


plot_SIDR <- function(popn, params) {
    message("Plotting SIDR model")

    N <- popn[sdp == "progeny", .N]
    tmax <- params$tmax

    events <- make_time_series_sidr(popn, params)

    ggplot(events) +
        aes(x = time,
            y = value / N,
            colour = variable) +
        geom_line(linewidth = 1.2) +
        scale_colour_manual("Compartments",
                            breaks = c("S", "I", "D", "ID", "R"),
                            labels = c("Susceptible (S)", "Undetectable (I)",
                                       "Detectable (D)", "Infectious (I+D)",
                                       "Removed (R)"),
                            values = c("blue", "mediumpurple", "purple", "red", "green")) +
        coord_cartesian(xlim = c(0, min(tmax, max(events$time), na.rm = TRUE)),
                        ylim = c(0, 1)) +
        labs(x = "Time (days)",
             y = "Proportion",
             title = "SIDR model") +
        theme_bw()
}


plot_SEIR <- function(popn, params) {
    message("Plotting SEIR model")

    N <- popn[sdp == "progeny", .N]
    tmax <- params$tmax

    events <- make_time_series_seir(popn, params)

    ggplot(events) +
        aes(x = time,
            y = value / N,
            colour = variable) +
        geom_line(linewidth = 1.2) +
        scale_colour_manual("Compartments",
                            breaks = c("S", "E", "I", "R"),
                            labels = c("Susceptible", "Exposed", "Infectious", "Removed"),
                            values = c("blue", "pink", "red", "green")) +
        coord_cartesian(xlim = c(0, min(tmax, max(events$time), na.rm = TRUE)),
                        ylim = c(0, 1)) +
        labs(x = "Time (days)",
             y = "Proportion",
             title = "SEIR model") +
        theme_bw()
}


plot_SIR <- function(popn, params) {
    message("Plotting SIR model")

    N <- popn[sdp == "progeny", .N]
    tmax <- params$tmax

    events <- make_time_series_sir(popn, params)

    ggplot(events) +
        aes(x = time,
            y = value / N,
            colour = variable) +
        geom_line(linewidth = 1.2) +
        scale_colour_manual("Compartments",
                            breaks = c("S", "I", "R"),
                            labels = c("Susceptible", "Infectious", "Removed"),
                            values = c("blue", "red", "green")) +
        coord_cartesian(xlim = c(0, min(tmax, max(events$time), na.rm = TRUE)),
                        ylim = c(0, 1)) +
        labs(x = "Time (days)",
             y = "Proportion",
             title = "SIR model") +
        theme_bw()
}


plot_SIS <- function(popn, params) {
    message("Plotting SIS model")

    N <- popn[sdp == "progeny", .N]
    tmax <- params$tmax

    events <- make_time_series_sis(popn, params)

    ggplot(events) +
        aes(x = time,
            y = value / N,
            colour = variable) +
        geom_line(linewidth = 1.2) +
        scale_colour_manual("Compartments",
                            breaks = c("S", "I"),
                            labels = c("Susceptible", "Infectious"),
                            values = c("blue", "red")) +
        coord_cartesian(xlim = c(0, min(tmax, max(events$time), na.rm = TRUE)),
                        ylim = c(0, 1)) +
        labs(x = "Time (days)",
             y = "Proportion",
             title = "SIS model") +
        theme_bw()
}
