# source("R/models/model_SI.R")
source("R/models/model_SIS.R")
source("R/models/model_SIR.R")
source("R/models/model_SEIR.R")
source("R/models/model_SIDR.R")
source("R/models/model_SEIDR.R")

#' Simulate an Epidemic Model
#'
#' Calls the correct model based on `params$model_type`.
#'
#' @param popn A population with trait values in data.table format
#' @param params A list of parameters
#'
#' @returns A new popn with epidemic event times, generation, and infectors
#' @export

simulate_epidemic <- function(popn, params) {
    switch(params$model_type,
           "SI"  = model_SI(popn, params), # not yet implemented
           "SIS" = model_SIS(popn, params),
           "SIR" = model_SIR(popn, params),
           "SEIR" = model_SEIR(popn, params),
           "SIDR" = model_SIDR(popn, params),
           "SEIDR" = model_SEIDR(popn, params))
}

