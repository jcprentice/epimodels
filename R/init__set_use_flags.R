set_use_flags <- function(params) {
    {
        all_traits    <- params$all_traits
        model_traits  <- params$model_traits
        IEs           <- params$IEs
        setup         <- params$setup
        LP_dist       <- params$LP_dist
        DP_dist       <- params$DP_dist
        RP_dist       <- params$RP_dist
        FEs           <- params$FEs
        setup         <- params$setup
        group_effect  <- params$group_effect
        # DTs are modified by reference, so this doesn't need to be copied back
        priors        <- params$priors
    }

    message("Setting use flags in priors ...")

    priors[, use := FALSE]

    # Turn on base parameters

    # get letters for each trait
    used <- intersect(names(model_traits), str_chars(IEs))

    use_parameters <- c(
        "beta",
        if ("l" %in% used) "LP",
        if ("d" %in% used) "DP",
        if ("t" %in% used) "RP",
        if ("l" %in% used && LP_dist == "gamma") "LP_shape",
        if ("d" %in% used && DP_dist == "gamma") "DP_shape",
        if ("t" %in% used && RP_dist == "gamma") "RP_shape",
        if (group_effect > 0) "sigma"
    )
    priors[parameter %in% use_parameters, use := TRUE]


    # Weight should not be nested unless setup is for 2+ trials
    if (!str_detect(setup, "_12")) {
        params$weight_is_nested <- FALSE
    }

    # Only use traits if in used AND in link_traits
    # GE_traits <- intersect(used, uniq_chars(link_traits))
    GE_traits <- used

    pwalk(expand.grid(x = used, y = used), \(x, y) {
        priors[str_ends(parameter, str_glue("_[GE]_{x}{y}")),
               use := all(c(x, y) %in% GE_traits)]
    })

    # Fixed effects
    walk(names(FEs), \(FE) {
        FE_str <- str_c(FE, "_", uniq_chars(FEs[[FE]]))
        priors[parameter %in% FE_str, use := TRUE]
    })


    params
}
