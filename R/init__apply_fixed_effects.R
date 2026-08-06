#' Apply trial, donor, and weight FEs to popn.
#'
#' @description `apply_fixed_effects()` takes a population data.table and a
#'   parameters list and applies all the fixed effects defined by `sim_X_fe`
#'   according to the FE values contained in `fe_vals`.
#'
#' @param popn A population data.table
#' @param params A list of parameters
#'
#' @returns A new population file with FEs applied to phenotypes

apply_fixed_effects <- function(popn, params) {
    {
        model_traits     <- params$model_traits
        sim_trial_fe     <- params$sim_trial_fe
        sim_donor_fe     <- params$sim_donor_fe
        sim_txd_fe       <- params$sim_txd_fe
        sim_weight_fe    <- params$sim_weight_fe
        fes              <- params$fes
        fe_vals          <- params$fe_vals
        use_weight       <- params$use_weight
        weight_is_nested <- params$weight_is_nested
    }

    message("Applying fixed effects to popn...")

    popn2 <- copy(popn)

    # Some traits don't have GVs and EVs, so we give placeholders with value 0

    traits_g <- str_c(model_traits, "_g")
    traits_e <- str_c(model_traits, "_e")
    missing_GEVs <- setdiff(c(traits_g, traits_e),
                            names(popn2))
    popn2[, (missing_GEVs) := 0]

    mat <- matrix(0,
                  nrow = popn2[sdp == "progeny", .N],
                  ncol = length(params$fes),
                  dimnames = list(NULL, names(params$fes)))

    walk(names(fes), \(fe) {
        mat[, fe] <<- if (weight_is_nested && str_detect(fe, "weight")) {
            tr <- fe |> str_extract("\\d+") |> as.integer()
            popn2[sdp == "progeny", recentre(log(weight)), trial] |>
                _[, fifelse(trial == tr, V1, 0)]
        } else if (str_detect(fe, "weight")) {
            popn2[sdp == "progeny", recentre(log(weight))]
        } else if (str_detect(fe, "trial")) {
            popn2[sdp == "progeny", recentre(trial == 2L)]
        } else if (str_detect(fe, "donor")) {
            popn2[sdp == "progeny", recentre(donor == 1L)]
        } else if (str_detect(fe, "txd")) {
            popn2[sdp == "progeny", recentre(trial == 2L & donor == 1L)]
        }
    })

    mat_fe <- mat %*% fe_vals[, model_traits]

    mat_g <- popn2[sdp == "progeny", ..traits_g] |> as.matrix()
    mat_e <- popn2[sdp == "progeny", ..traits_e] |> as.matrix()

    # Calculate the phenotype
    mat_pt <- exp(mat_g + mat_e + mat_fe)
    colnames(mat_pt) <- model_traits

    popn2[sdp == "progeny", (model_traits) := as.data.frame(mat_pt)]

    popn2[, (missing_GEVs) := NULL]

    popn2
}
