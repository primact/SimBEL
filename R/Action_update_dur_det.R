# 10/03/2017 Guillaume de Kervenoael

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_dur_det_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante du portefeuille action suite a la vente de tout ou partie de ce portefeuille.
##'
##' \code{update_dur_det_action} est une methode permettant de mettre a jour la duree de detention des composantes d'un portefeuille Action.
##' @name update_dur_det_action
##' @docType methods
##' @param x objet de la classe \code{Action} (decrivant le portefeuille action en detention).
##' @return L'objet \code{x} mis a jour du vieillissement de la duree de detention.
##' @author Prim'Act
##' @export
##' @aliases Action
##' @include Action_class.R


setGeneric(name = "update_dur_det_action", def = function(x){standardGeneric("update_dur_det_action")})
setMethod(
    f = "update_dur_det_action",
    signature = "Action",
    definition = function(x){
        if(nrow(x["ptf_action"]) == 0) { cat("[WARNING : Action : update_dur_det_action] : Le portefeuille action initial est vide. \n")
            return(x)}
        x["ptf_action"][,"dur_det"] <- x["ptf_action"][,"dur_det"] + 1
        return(x)
    }
)
