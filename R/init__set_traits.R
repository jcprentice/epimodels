#' Set call appropriate function depending on the `trait_source`
#'
#' @param popn A pedigree data.table
#' @param params A list of parameters
#'
#' @returns A new `popn` data.table.
#' @export
set_traits <- function(popn, params) {
    switch(stringr::str_to_lower(params$traits_source),
           "posterior" = patch_in_traits(popn, params),
           "grm"       = make_traits_from_grm(popn, params),
           "pedigree"  = make_traits_from_pedigree(popn, params),
           popn)
}

