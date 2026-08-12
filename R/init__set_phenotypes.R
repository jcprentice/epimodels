#' Apply trial, donor, and weight FEs to popn.
#'
#' @description `set_phenotypes()` takes a population data.table and a
#'   parameters list and applies all the fixed effects defined by `sim_X_fe`
#'   according to the FE values contained in `FE_vals`.
#'
#' @param popn A population data.table
#' @param params A list of parameters
#'
#' @returns A new population file with FEs applied to phenotypes

set_phenotypes <- function(popn, params) {
    {
        model_traits     <- params$model_traits
        FEs              <- params$FEs
        FE_vals          <- params$FE_vals
        use_weight       <- params$use_weight
        weight_is_nested <- params$weight_is_nested
    }

    message("- Applying fixed effects to popn...")

    popn2 <- copy(popn)

    # Some traits don't have GVs and EVs, so we give placeholders with value 0

    traits_g <- str_c(model_traits, "_g")
    traits_e <- str_c(model_traits, "_e")
    missing_GEVs <- setdiff(c(traits_g, traits_e),
                            names(popn2))
    popn2[, (missing_GEVs) := 0]

    mat <- matrix(0,
                  nrow = popn2[sdp == "progeny", .N],
                  ncol = length(FEs),
                  dimnames = list(NULL, names(FEs)))

    walk(names(FEs), \(FE) {
        mat[, FE] <<- if (weight_is_nested && str_detect(FE, "weight\\d")) {
            tr <- FE |> str_extract("\\d+") |> as.integer()
            popn2[sdp == "progeny", recentre(log(weight)), trial] |>
                _[, fifelse(trial == tr, V1, 0)]
        } else if (str_detect(FE, "weight")) {
            popn2[sdp == "progeny", recentre(log(weight))]
        } else if (str_detect(FE, "trial")) {
            popn2[sdp == "progeny", recentre(trial == 2L)]
        } else if (str_detect(FE, "donor")) {
            popn2[sdp == "progeny", recentre(donor == 1L)]
        } else if (str_detect(FE, "txd")) {
            popn2[sdp == "progeny", recentre(trial == 2L & donor == 1L)]
        } else {
            0
        }
    })

    mat_fe <- mat %*% FE_vals[, model_traits]

    mat_g <- popn2[sdp == "progeny", ..traits_g] |> as.matrix()
    mat_e <- popn2[sdp == "progeny", ..traits_e] |> as.matrix()

    # Calculate the phenotype
    message(" - Setting individual phenotypes ...")
    mat_pt <- exp(mat_g + mat_e + mat_fe)
    colnames(mat_pt) <- model_traits

    popn2[sdp == "progeny", (model_traits) := as.data.frame(mat_pt)]

    popn2[, (missing_GEVs) := NULL]

    popn2
}
