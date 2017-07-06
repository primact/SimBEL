# 10/03/2017 Guillaume de Kervenoael, Quentin Guibert
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_frais_fin
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul des frais financier.
##'
##' \code{calc_frais_fin} est une methode permettant de calculer les frais financiers.
##' @name calc_frais_fin
##' @docType methods
##' @param x est un objet de type \code{FraisFin} contenant les parametres des frais financiers associes a un canton.
##' @param vm_moy est un objet de type \code{numeric} correspondant a la valeur moyenne de l'actif en valeur
##' de marche.
##' @param coef_inflation est un objet de type \code{numeric} correspondant au coefficient d'inflation des frais.
##' @return La valeur des frais financiers : un reel de type \code{numeric}.
##' @author Prim'Act
##' @export
##' @include FraisFin_class.R

setGeneric(name = "calc_frais_fin", def = function(x, vm_moy, coef_inflation){standardGeneric("calc_frais_fin")})
setMethod(
    f = "calc_frais_fin",
    signature = c(x = "FraisFin", vm_moy = "numeric", coef_inflation ="numeric"),
    definition = function(x, vm_moy, coef_inflation){
        
        # Verification des inputs
        len_inflation <- length(coef_inflation)
        if (length(vm_moy) != len_inflation | len_inflation != 1L) stop("[Frais : calc_frais] : Les inputs ne sont pas de la meme dimension \n")
        
        # Calcul des frais
        ind_inflation <- x@indicatrice_inflation
        return (x@tx_chargement * vm_moy * ((! ind_inflation) + coef_inflation * ind_inflation))
    }
)
