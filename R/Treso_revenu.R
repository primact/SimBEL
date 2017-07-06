
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           revenu_treso
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le revenu tresorerie.
##'
##' \code{revenu_treso} est une methode permettant de calculer le revenu de la tresorerie.
##' @name revenu_treso
##' @docType methods
##' @param x est un objet de la classe \code{\link{Treso}} en debut d'annee.
##' @param rdt est le rendement de la classe \code{\link{Treso}} au cours de l'annee (i.e. en fin d'annee)
##' @param flux_milieu est le flux du milieu de l'annee en cours.
##' @return Le montant du revenu.
##' @author Prim'Act
##' @export
##' @include Treso_class.R

setGeneric(name = "revenu_treso", def = function(x, rdt, flux_milieu){standardGeneric("revenu_treso")})
setMethod(
    f = "revenu_treso",
    signature = c(x = "Treso", rdt = "numeric", flux_milieu = "numeric" ),
    definition = function(x, rdt, flux_milieu){
        
        # Verification des inputs
        len_rdt <- length(rdt)
        if (nrow(x["ptf_treso"]) != len_rdt | len_rdt != length(flux_milieu)) 
            stop("[Treso : revenu_treso] : Les inputs ont des dimensions distinctes.")
        
        # Calcul du vecteur de revenu : ATTENTION, il est essentiel que le portefeuille treso soit anterieur au flux et au rdt :
        # on calcule le revenu  du ptf sur la VM n-1
        revenu = x["ptf_treso"][,"val_marche"] * rdt + flux_milieu * (sqrt(1 + rdt) - 1)
        
        # Output
        return(revenu)
    }
)
