
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           revenu_treso
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul le revenu tresorerie.
##'
##' \code{revenu_treso} est une methode permettant de calculer les valeurs de marche.
##' @name revenu_treso
##' @docType methods
##' @param x est un objet de la classe Treso en debut d'annee
##' @param rdt est le rendement de la classe Treso au cours de l'annee (i.e. en fin d'annee)
##' @param flux_milieu est le flux du milieu de l'annee en cours (i.e. ulterieur a l'objet Treso renseigne)
##' @return
##' @author Prim'Act
##' @export
##' @aliases Treso
##' @include Treso_class.R

setGeneric(name = "revenu_treso", def = function(x, rdt, flux_milieu){standardGeneric("revenu_treso")})
setMethod(
    f = "revenu_treso",
    signature = c(x = "Treso", rdt = "numeric", flux_milieu = "numeric" ),
    definition = function(x, rdt, flux_milieu){
        # Verification des inputs
        if (nrow(x["ptf_treso"]) != length(rdt) | length(rdt) != length(flux_milieu)) {stop("[Treso : revenu_treso] : Les inputs ont des dimensions distinctes.")}
        # Calcul du vecteur de revenu : ATTENTION, il est essentiel que le portefeuille treso soit anterieur au flux et au rdt :
        # on calcule le revenu  du ptf sur la VM n-1
        revenu = x["ptf_treso"][,"val_marche"] * rdt + flux_milieu * ((1 + rdt)^0.5 - 1)
        return(revenu)
    }
)
