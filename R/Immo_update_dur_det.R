# 10/03/2017 Guillaume de Kervenoael

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_dur_det_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour des durees de detention des composantes d'un portefeuille immobilier.
##'
##' \code{update_dur_det_immo} est une methode permettant de mettre a jour la duree de detention des composantes d'un portefeuille immobilier.
##' @name update_dur_det_immo
##' @docType methods
##' @param x objet de la classe \code{Immo} (decrivant le portefeuille immo en detention).
##' @return L'objet \code{x} mis a jour du vieillissement de la duree de detention.
##' @author Prim'Act
##' @export
##' @aliases Immo
##' @include Immo_class.R

setGeneric(name = "update_dur_det_immo", def = function(x){standardGeneric("update_dur_det_immo")})
setMethod(
    f = "update_dur_det_immo",
    signature = "Immo",
    definition = function(x){
        if(nrow(x["ptf_immo"]) == 0) {cat("[WARNING : Immo : update_dur_det_immo] : Le portefeuille immo initial est vide. \n")
            return(x)}
        x["ptf_immo"][,"dur_det"] <- x["ptf_immo"][,"dur_det"] + 1
        return(x)
    }
)
