#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_reserves
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Evalue et met a jour la valeur des autres reserves.
##'
##' \code{update_reserves} est une methode permettant de calculer la valeur de la nouvelle PGG et de la nouvelle
##' PSAP et les met a jour.
##' @name update_reserves
##' @docType methods
##' @param x objet de la classe \code{\link{AutresReserves}}.
##' @param prest_ep est une valeur \code{numeric} correspondant a la somme des prestations nettes de chargement et
##' de charges sociales sur epargne.
##' @param prest_autres est une valeur \code{numeric} correspondant a la somme des prestations nettes
##'  de chargements et de charges sociales sur autres passifs.
##' @param pm_ep est une valeur \code{numeric}  correspondant a la somme des PM nettes de chargements et
##' de charges sociales sur epargne.
##' @param pm_autres est une valeur \code{numeric} correspondant a la somme des PM nettes de chargement et
##' de charges sociales sur autres passifs.
##' @return \code{x} l'objet\code{\link{AutresReserves}} mis a jour.
##' @return \code{var_psap} une valeur \code{numeric} correspondant a la variation de PSAP.
##' @return \code{var_gg} une valeur \code{numeric} correspondant a la variation de PGG.
##' @note Il s'agit d'une methode simplifiee.
##' @author Prim'Act
##' @export
##' @include AutresReserves-class.R

setGeneric(name = "update_reserves", def = function(x, prest_ep, prest_autres, pm_ep, pm_autres){
  standardGeneric("update_reserves")})
setMethod(
  f = "update_reserves",
  signature = c(x = "AutresReserves",
                prest_ep = "numeric",
                prest_autres = "numeric",
                pm_ep = "numeric",
                pm_autres = "numeric"),
  definition = function(x, prest_ep, prest_autres, pm_ep, pm_autres){

    # Test
    if (length(prest_ep) != length(prest_autres) | length(prest_autres) != length(pm_ep) |
        length(pm_ep) != length(pm_autres) |
        length(pm_autres) != 1) {stop("[PortPassif : update_reservs] : Les vecteurs d'input doivent etre de longueur 1. \n")}

    # Mise a jour de la valeur de la PSAP et de la PGG
    # Application d'un proxy propre a la MGP
    x["psap_valeur"] <- x@tx_psap_ep * prest_ep + x@tx_psap_autres * prest_autres
    x["pgg_valeur"] <- x@tx_pgg_ep * pm_ep + x@tx_pgg_autres * pm_autres

    # Calcul de la variation sur la periode
    var_psap <- x@psap_valeur - x@psap_debut
    var_pgg <- x@pgg_valeur - x@pgg_debut

    # Output
    return(list(x = x, var_psap = var_psap, var_pgg = var_pgg))
  }
)
