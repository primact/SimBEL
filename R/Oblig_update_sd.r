# 10/03/2017 Guillaume de Kervenoael

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_sd_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour des surcotes decotes d'un portefeuille obligataire.
##'
##' \code{update_sd_oblig} est une methode permettant de mettre a jour la surcotes decotes des composantes d'un portefeuille obligataire.
##' @name update_sd_oblig
##' @docType methods
##' @param x objet de la classe \code{Oblig} (decrivant le portefeuille obligataire en detention).
##' @param sd un vecteur de \code{numeric} a assigner a l'objet \code{Obligation}. 
##' @return L'objet \code{x} dont les surcotes decotes ont ete mises a jour.
##' @author Prim'Act
##' @export
##' @aliases Oblig
##' @include Oblig_class.R

setGeneric(name = "update_sd_oblig", def = function(x,sd){standardGeneric("update_sd_oblig")})
setMethod(
    f = "update_sd_oblig",
    signature = c(x = "Oblig", sd = "numeric"),
    definition = function(x, sd){
        # Verification des inputs
        if (nrow(x["ptf_oblig"]) != length(sd)) { stop("[Oblig : update_oblig] Les inputs ne sont pas de memes dimensions")}
        x["ptf_oblig"][,"sd"] <- sd
        return(x)
    }
)