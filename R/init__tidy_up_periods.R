
#' Tidy up periods
#'
#' If modifying the `params` list (likely with a `protocol` list) touches any of
#' the TPs or shape parameters, make sure that these changes are reflected in
#' the rate parameters and the priors.
#'
#' @param params A list of parameters
#' @param protocol A list to override params
#'
#' @returns A new `params` list
#' @export

tidy_up_periods <- function(params, protocol = NULL) {
    params2      <- copy(params)
    sim_new_data <- params$sim_new_data

    TPs <- if (is.null(protocol)) {
        c("LP", "LP_shape", "DP", "DP_shape", "RP", "RP_shape")
    } else {
        names(protocol)
    }

    # Fix TP scales
    params2$LP_scale <- with(params2, LP / LP_shape)
    params2$DP_scale <- with(params2, DP / DP_shape)
    params2$RP_scale <- with(params2, RP / RP_shape)

    # Update the TP priors
    walk(TPs,
         ~ params2$priors[parameter == .x, `:=`(
             val1 = min(params2[[.x]], val1),
             val2 = max(params2[[.x]], val2),
             true_val = params2[[.x]]
         )]
    )

    params2
}
