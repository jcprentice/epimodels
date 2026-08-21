#' Generate a list of parameters
#'
#' @description Generate a complete list of parameters, factoring in arguments
#'   provided. Values can be modified later if preferred, but be careful about
#'   maintaining consistency.
#'
#' @param model_type Epidemic model: "SIR", "SIDR", "SEIR", "SEIDR"
#' @param dataset Name for overall dataset (default "testing")
#' @param name Name of scenario within dataset (default "scen-1-1")
#' @param scenario Integer for scenario
#' @param replicate Integer for replicate
#' @param setup Population layout
#' @param group_layout How to arrange individuals into groups: "random",
#'   "striped", "family", "fishboost"
#' @param IEs Which traits are affected by individual genetic effects, either in
#'   simulation or inference. This should correspond to the names of the model
#'   traits used. Use a trait's first letter if used, or _ if unused. For linked
#'   traits use the letter of the trait it's linked to. "", "all", or "none" are
#'   also accepted, with the obvious meanings/
#' @param vars Variances: diagonal of covariance matrix, either
#' * a single value, e.g. 1.0
#' * a list wrapped vector of compatible length, e.g.
#'     * list(1, 1.5, 0, 0, 0.5)
#' * a list wrapped named vector e.g.
#'     * list(sus = 1, inf = 1.5, tol = 0.5, default = 0)
#' @param cors Correlations for Sigma matrix, either
#' * a single numerical value, e.g. 0.2
#' * a named list-wrapped vector, e.g.
#'     * list(si = 0.3, st = -0.1, it = 0.2, default = 0)
#' * a named list of compatible with the lower triangle order, e.g.
#'     * list(si, sl, sd, st, il, id, it, ld, lt, dt)
#' @param FEs A list of Fixed Effect names. Values should correspond to the full
#'   list of model traits they apply to, e.g. for an SEIR model with traits
#'   "silt" use:
#'   list(sex = "silt",   # all traits used
#'        trial = "ilt",  # susceptibility not used
#'        weight = "sit") # latency not used
#' @param group_effect Include group effect if >= 0, simulate if appropriate and
#'   check, ignore if < 0
#' @param sim_new_data Simulate new data in "r" or "bici". Alternatively, "no"
#'   for using FB data, or "summary_sim" or "summary_inf" for getting data from
#'   an existing dataset (specified in patches)
#'
#' @returns A list containing all the parameters
#' @export

make_parameters <- function(
        model_type = "SEIR", name = "scen-1-1", dataset = "testing",
        scenario = 1L, replicate = 1L, setup = "fb_12_rpw", group_layout = "fishboost",
        IEs = "sit", vars = 1.0, cors = 0.2,
        FEs = list(sex = "sit", age = "sit", weight = "sit"),
        group_effect = -1,
        sim_new_data = "r",
        protocol = NULL
) {
    message("Setting parameters ...")

    # Fix input NAs ----

    # Handy in case of debugging
    if (FALSE) {
        model_type <- "SEIDR"; dataset <- "testing"; name <- "scen-1-1";
        scenario <- 1L; replicate = 1L; setup <- "fb_12_rpw"; IEs <- "sit";
        vars <- 1; cors <- 0.2; group_layout <- "fishboost"; group_effect <- -1;
        FEs <- list(trial = "ildt", donor = "ildt", txd = "ildt",
                    weight1 = "sildt", weight2 = "sildt");
        sim_new_data <- "r";
    }

    # For importing from a protocol file. Any parameters that just need defining
    # can be added here, but others that need post-processing will be defined
    # later.
    if (!missing(protocol) && !is.null(protocol)) {
        message("- using Protocol for args")
        for (key in names(protocol)) {
            val <- protocol[[key]]
            if (length(val) > 1 || !is.na(val)) {
                assign(key, val)
            }
        }
    }

    # Fix any parameters incorrectly assigned as NULL
    {
        if (missing(model_type))   model_type <- "SEIDR"
        if (missing(name))         name <- "scen-1-1"
        if (missing(dataset))      dataset <- "testing"
        if (missing(scenario))     scenario <- 1L
        if (missing(replicate))    replicate <- 1L
        if (missing(setup))        setup <- "fb_12_rpw"
        if (missing(IEs))          IEs <- "si__t"
        if (missing(vars))         vars <- 0
        if (missing(cors))         cors <- 0
        if (missing(group_layout)) group_layout <- "fishboost"
        if (missing(FEs))          FEs <- list()
        if (missing(sim_new_data)) sim_new_data <- "r"
    }

    # Description
    description <- "Basic test model"

    label <- stringr::str_c("s", scenario)

    # Set output directories ----

    # Directories should be something like "dataset/data", "dataset/results", or
    # "testing" if just testing (and convert to basic string)
    if (dataset == "") {
        dataset <- "testing"
    }

    {
        base_dir    <- c(stringr::str_glue("datasets/{dataset}"))
        gfx_dir     <- c(stringr::str_glue("{base_dir}/gfx"))
        data_dir    <- c(stringr::str_glue("{base_dir}/data"))
        results_dir <- c(stringr::str_glue("{base_dir}/results"))
        meta_dir    <- c(stringr::str_glue("{base_dir}/meta"))
        config      <- c(stringr::str_glue("{data_dir}/{name}.bici"))
        output_dir  <- c(stringr::str_glue("{data_dir}/{name}-out"))
        states_dir  <- c(stringr::str_glue("{output_dir}/states"))
    }

    # Population setup ----
    message(stringr::str_glue("- Setup is: '{setup}'\n",
                     "- Group layout is: '{group_layout}'"))

    # Note: the FB dataset isn't balanced
    # otherwise dpsire = dams per sire, ppdam = progeny per dam
    tmp <- switch(
        setup,           # sires dams  progeny groups I0
        "fb_12_rpw"    = c(28,   25,   1750,   70,    5),
        "fb_1_rpw"     = c(14,   14,   875,    35,    5),
        "fb_2_rpw"     = c(17,   14,   875,    35,    5),
        "large"        = c(100,  2000, 2000,   200,   5),
        "small"        = c(3,    6,    12,     4,     1),
        "single"       = c(10,   20,   500,    1,     5),
        "multiple"     = c(10,   20,   500,    20,    5),
                         c(28,   25,   1750,   70,    5)) |>
        as.integer() |>
        setNames(c("nsires", "ndams", "nprogeny", "ngroups", "I0")) |>
        as.list()

    nsires <- tmp$nsires; ndams <- tmp$ndams; nprogeny <- tmp$nprogeny
    ngroups <- tmp$ngroups; I0 <- tmp$I0


    iceil <- function(x) as.integer(ceiling(x))

    # Derived numbers
    {
        nparents <- nsires + ndams
        ntotal <- nprogeny + nparents
        dpsire <- iceil(ndams / nsires)
        ppdam <- iceil(nprogeny / ndams)
        group_size <- iceil(nprogeny / ngroups)
    }

    # Traits ----
    message(stringr::str_glue("- Model type is: '{model_type}'"))

    # Compartments are just the letters in model_type (ignore repeated S)
    compartments <- uniq_chars(model_type)

    all_traits <- c(s = "sus", i = "inf", l = "lat", d = "det", t = "tol")

    # Set up which traits a model will use, if traits are +vely or -vely
    # correlated, what the event times are called, and which parameters we need
    # to make BICI aware of.
    switch(model_type,
           "SEIDR" = {
               model_traits <- all_traits
               timings <- c("Tinf", "Tinc", "Tsign", "Tdeath")
           }, "SIDR" = {
               model_traits <- all_traits[c("s", "i", "d", "t")]
               timings <- c("Tinf", "Tsign", "Tdeath")
           }, "SEIR" = {
               model_traits <- all_traits[c("s", "i", "l", "t")]
               timings <- c("Tinf", "Tsign", "Tdeath")
           }, "SIR" = {
               model_traits <- all_traits[c("s", "i", "t")]
               timings <- c("Tinf", "Tdeath")
           }, "SIS" = {
               model_traits <- all_traits[c("s", "i", "t")]
               timings <- c("Tinf", "Tdeath")
           }, "SI" = {
               model_traits <- all_traits[c("s", "i")]
               timings <- c("Tinf")
           }, {
               stop("- Unknown model!")
        })

    ## Genetic and Environmental covariances ----

    # Likely using only a subset of traits
    IEs <- if (IEs %in% c("", "none", NA)) {
      strrep("_", length(model_traits))
    } else if (IEs == "all") {
      model_traits |> names() |> stringr::str_flatten()
    } else {
      stringr::str_to_lower(IEs)
    }

    IEs_used <- IEs |> str_chars() |>
      stringr::str_subset("_", negate = TRUE) |> stringr::str_flatten()

    n_traits_used <- stringr::str_length(IEs_used)
    traits_used <- model_traits[str_chars(IEs_used)]

    if (n_traits_used > 0) {
        message("- ", n_traits_used, " genetic traits: ",
                stringr::str_flatten_comma(traits_used))
    } else {
        message("- 0 genetic traits used")
    }

    ## Covariance matrices ----

    out <- make_matrices(model_traits, IEs_used, vars, cors)
    Sigma_G <- Sigma_E <- out$Sigma
    cov_G <- cov_E <- out$cov

    # This should expand vars and cors to be the full matrix values, possibly
    # with replicates
    vars <- Sigma_G |> diag()
    cors <- Sigma_G[lower.tri(Sigma_G)] |> setNames(out$cor_names)


    # Epidemic parameters ----

    ## Infection coefficient ----
    r_beta <- 0.5

    # Infectivity model
    # 1: standard
    # 2: I is 10% as infectious as D
    # 3: donors are 10% as infectious as recipients
    # 4: donors are `id_ratio` as infectious as recipients
    inf_model <- 1L

    # Infectivity ratio
    inf_ratio <- if (inf_model == 1) 1 else 0.1

    # Note for Gamma dist:
    # shape = mean^2 / var
    # rate  = mean / var

    ## Compartment Periods ----

    ### Latency period E->I ----

    # Calculated for an SEIDR model based on donors in trial 1 (+ 0.5)
    # fitdist(Tsign + 0.5, "gamma")
    LP <- 10
    LP_shape <- 1 # 1.46 # 2.0 # 10.0
    LP_scale <- LP / LP_shape # 0.232 # LP_shape / LP
    LP_dist <- "exp"


    ### Detection period I->D ----

    # This is currently identical to the latency
    DP <- 10
    DP_shape <- 1 # 1.46 # 2.0 # 10.0
    DP_scale <- DP / DP_shape # 0.232 # LP_shape / LP
    DP_dist <- "exp"


    ### Removal ----

    # fitdist(RP + 0.5, "gamma")
    RP <- 10
    RP_shape <- 1
    RP_scale <- RP / RP_shape # 0.149
    RP_dist <- "exp"


    ## Fixed effects ----

    # A default set of FEs
    FE_vals <- matrix(0,
                      nrow = length(FEs),
                      ncol = length(model_traits),
                      dimnames = list(FE = names(FEs),
                                      traits = unname(model_traits)))

    ## R0 ----

    # This should target R0 around 5?
    R0 <- r_beta * (RP + if ("D" %in% compartments) DP else 0)


    # MCMC settings ----

    bici_cmd <- "inf"

    ## Samples ----
    nchains <- 16L
    nsample  <- 1e4 # total no. of samples BICI should take
    burnprop <- 0.2 # burn-in proportion of chain
    thinto   <- 1e4 # no. of samples to output
    burnin   <- nsample * burnprop
    thin     <- max(nsample / thinto, 1)

    # This should be tweaked to give ~ 25% anneal time
    nsample_per_gen <- max(3e-3 * nsample, 1)

    sample_states <- 0L # how many states (per chain) to sample
    ie_output <- "true" # if ss > 0, include IE samples in extended_trace_combine.tsv

    # Should BICI use the PAS method (N chains for a single call) or regular MCMC?
    algorithm    <- "pas" # "mcmc"

    phi <- 1.0

    # Priors ----

    ge <- max(group_effect, 0)

    # What kind of prior do we want?
    cov_prior <- list(type = "default")
    # cov_prior <- list(type = "normal-lkj", vals = (2, 1.2))
    # cov_prior <- list(type = "uniform-lkj", vals = (1e-4, 4, 1.2))
    # cov_prior <- list(type = "inv-wishart", vals = c(S, nu))

    single_prior <- c("inverse", "uniform")[[1]]

    ## Base priors ----
    base_priors <- rowwiseDT(
        parameter=, true_val=, type=,        val1=, val2=,

        "beta",     r_beta,    single_prior, 0,     2.0,
        "LP",       LP,        single_prior, 0,     40,
        "DP",       DP,        single_prior, 0,     160,
        "RP",       RP,        single_prior, 0,     20,
        "LP_shape", LP_shape,  "uniform",    0.5,   5,
        "DP_shape", DP_shape,  "uniform",    0.5,   5,
        "RP_shape", RP_shape,  "uniform",    0.5,   5,
        "infrat",   inf_ratio, "uniform",    0,     2,
        "sigma",    ge,        single_prior, 0,     2 * ge)

    ## BICI priors ----

    bici_priors <- rowwiseDT(
        parameter=,   true_val=, type=,        val1=, val2=,
        "beta_Tr1",   r_beta,    single_prior, 0,   2,
        "beta_Tr2",   r_beta,    single_prior, 0,   2,
        "LP_Tr1,Don", LP,        single_prior, 0,   50,
        "LP_Tr1,Rec", LP,        single_prior, 0,   50,
        "LP_Tr2,Don", LP,        single_prior, 0,   120,
        "LP_Tr2,Rec", LP,        single_prior, 0,   50,
        "DP_Tr1,Don", DP,        single_prior, 0,   50,
        "DP_Tr1,Rec", DP,        single_prior, 0,   50,
        "DP_Tr2,Don", DP,        single_prior, 0,   160,
        "DP_Tr2,Rec", DP,        single_prior, 0,   50,
        "RP_Tr1,Don", RP,        single_prior, 0,   30,
        "RP_Tr1,Rec", RP,        single_prior, 0,   30,
        "RP_Tr2,Don", RP,        single_prior, 0,   30,
        "RP_Tr2,Rec", RP,        single_prior, 0,   30)

    ## Cov priors ----
    t1 <- model_traits |> names()
    xx <- strrep(t1, 2)
    xy <- expand.grid(t1, t1) |> rev() |> apply(1, stringr::str_flatten) |>
        matrix(nrow = length(t1)) |> pipebind::bind(x, x[lower.tri(x)])
    cov_p <- c(stringr::str_c("cov_G_", xx), stringr::str_c("r_G_", xy),
               stringr::str_c("cov_E_", xx), stringr::str_c("r_E_", xy))

    cov_priors <- data.table(parameter = cov_p, true_val = 0,
                             type = "default", val1 = 0, val2 = 4)
    cov_priors[stringr::str_starts(parameter, "r_"),
               `:=`(val1 = -0.9, val2 = 0.9)]

    ## FE priors ----

    FE_p <- expand.grid(t1, names(FEs)) |> rev() |>
      apply(1, stringr::str_flatten, "_")
    FE_priors <- data.table(parameter = FE_p, true_val = 0,
                            type = "uniform", val1 = -4, val2 = 4)

    priors <- rbind(base_priors, bici_priors, cov_priors, FE_priors,
                    fill = TRUE)

    ## Tidy up ----

    # Roughly fix some priors
    widen_priors(priors)

    # Set everything to off as standard, then turn on as necessary
    priors[, use := FALSE]

    # This will be all be redone with set_use_flags(), but make_parameters()
    # should give as correct a result as possible to start off with.

    use_parameters <- c(
        "beta",
        if ("l" %in% t1) "LP",
        if ("d" %in% t1) "DP",
        if ("t" %in% t1) "RP",
        if ("l" %in% t1 && LP_dist == "gamma") "LP_shape",
        if ("d" %in% t1 && DP_dist == "gamma") "DP_shape",
        if ("p" %in% t1 && RP_dist == "gamma") "RP_shape",
        if (group_effect > 0) "sigma"
    )
    priors[parameter %in% use_parameters, use := TRUE]

    GE_traits <- t1 |>
        intersect(str_chars(IEs_used))

    purrr::pwalk(expand.grid(x = t1, y = t1), \(x, y) {
        priors[stringr::str_ends(parameter, stringr::str_glue("_[GE]_{x}{y}")),
               use := all(c(x, y) %in% GE_traits)]
    })


    # Additional settings ----

    # Important for SIS model which may not end otherwise
    t_end <- 100

    popn_format <- c("intervals", "times")[[2]]

    seed <- 0

    # Number of times to simulate in BICI
    nreps <- 50

    # pass event times, pass the last N of timings (Tinf, Tinc, Tsign, Tdeath)
    pass_events <- c("Tsign", "Tdeath") # pass time Tsign and Tdeath

    # time step between data
    time_step <- 0
    # time step for inference
    time_step_bici <- 1

    censor <- 1.0

    ## Fixes for FB data set
    fix_donors <- c() # c("time", "no_Tsign_survivors", "set_to_R")
    # All donors who fail to show visual signs by this point are demoted
    t_demote <- c(20, 80)
    # if Tsign == Tdeath, then bump Tsign back by 1/2
    fix_eq_time <- TRUE


    # Patch data with values from dataset / scenario posterior
    patch_dataset <- ""
    patch_name <- "scen-1-1"
    # Use mean / median / randomly sample from posterior
    patch_type <- c("mean", "median", "sampled")[[1]]
    # Use states to patch EBVs and parameters (otherwise use trace files)
    patch_state <- FALSE

    # Use weight if available
    use_weight <- "log" # "log" or "linear"

    # Use inverse H matrix, GRM, or none
    use_grm <- "" # {A,H} + {,_inv} + {,_nz}

    # How to build popn?
    # "pedigree" / "grm" = generate new values based on relationship
    # "posterior" = patch from posteriors given in "patch_xyz"
    traits_source <- c("pedigree", "grm", "posterior")[[1]]

    # Show plots during run
    show_plots <- TRUE

    # show messages
    msgs <- TRUE

    # Show details
    DEBUG <- 0


    # Create params list ----
    #
    # This will be a list for passing to functions
    params <- mget(
        # Inputs
        c("description", "dataset", "scenario", "name", "label", "replicate", "seed",
          "data_dir", "results_dir", "gfx_dir", "meta_dir", "output_dir", "states_dir", "config",
          "model_type", "sim_new_data", "setup", "vars", "cors", "group_layout", "IEs",
          # Population parameters
          "nsires", "ndams", "nparents", "nprogeny", "ngroups", "ntotal",
          "dpsire", "ppdam", "group_size", "I0",
          # Model traits
          "compartments", "timings", "all_traits", "model_traits", "FEs",
          "Sigma_E", "Sigma_G", "cov_G", "cov_E",
          # Main model parameters
          "r_beta", "inf_ratio", "inf_model", "group_effect", "R0", # infection
          "LP", "LP_shape", "LP_scale", "LP_dist", # latency
          "DP", "DP_shape", "DP_scale", "DP_dist", # detection
          "RP", "RP_shape", "RP_scale", "RP_dist", # removal
          "FE_vals",
          # MCMC & extra
          "priors", "cov_prior", "single_prior",
          "popn_format", "bici_cmd", "algorithm",
          "nchains", "nsample", "burnprop", "thinto", "nchains",
          "phi", "nsample_per_gen", "sample_states",
          "ie_output", "time_step", "time_step_bici", "censor", "nreps",
          "fix_donors", "t_demote", "fix_eq_time",
          "pass_events", "patch_dataset", "patch_name", "patch_type", "patch_state",
          "use_weight", "use_grm", "traits_source",
          "t_end", "show_plots", "msgs", "DEBUG"))

    params
}


# Handy summary ----
#' Summarise Parameters
#'
#' @param params A list of parameters
#'
#' @export

summarise_params <- function(params) {
    with(params, {
        if (sim_new_data == "no") {
            message("Using Fishboost data")
        } else if (sim_new_data == "summary_inf") {
            message("Using data simulated in BICI from prior")
        } else if (sim_new_data == "summary_ps") {
            message("Using data simulated in BICI from posterior")
        } else {
            message(stringr::str_glue("Simulating new {model_type} data via {x}",
                             x = stringr::str_to_upper(sim_new_data)))
        }

        message(stringr::str_glue(
            "- Demography is:",
            "\t{nsires} sires, {ndams} dams, {nprogeny} progeny ({ntotal} total)",
            "\t{ngroups} group{s} (group size {group_size})",
            s = if (ngroups > 1) "s" else "",
            .trim = FALSE, .sep = "\n"
        ))

        message(stringr::str_glue(
            "- Individual Effects on:",
            stringr::str_c("\t", if (IEs %in% c("", "none")) "none" else
                stringr::str_flatten_comma(model_traits[str_chars(IEs)])),
            .trim = FALSE, .sep = "\n"
        ))

        message("- Sigma_G: ")
        capture_message(Sigma_G[model_traits, model_traits])

        message("- Fixed Effects are:")


        message(stringr::str_glue(
            "- Fixed Effects are:",
            stringr::str_c(names(FEs), " = '", FEs, "'") |>
              stringr::str_flatten_comma(),.trim = FALSE, .sep = "\n"
        ))

        message(stringr::str_glue(
            "- Estimated R0:",
            "\t{r0}",
            r0 = signif(R0, 3),
            .trim = FALSE, .sep = "\n"
        ))

        if (patch_dataset != "") {
            message(stringr::str_glue(
                "- Patching with dataset {patch_dataset} / {patch_name}",
                "\ttype = {patch_type}, state = {patch_state}",
                .trim = FALSE, .sep = "\n"
            ))
        }

        message(stringr::str_glue(
            "- Running MCMC with:",
            "\t{ns} updates, {th} samples, {burnprop} burnin, {nchains} chains",
            "- BICI script file:",
            "\t'{data_dir}/{name}.bici'",
            "- Results file:",
            "\t'{results_dir}/{name}.rds'",
            ns = format(nsample, scientific = FALSE, big.mark = ","),
            th = format(thinto,  scientific = FALSE, big.mark = ","),
            .trim = FALSE, .sep = "\n"
        ))
    })
}
