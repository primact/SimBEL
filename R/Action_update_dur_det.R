#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_dur_det_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour des durees de detention d'un portefeuille action.
##'
##' \code{update_dur_det_action} est une methode permettant de mettre a jour la duree de detention des composantes d'un portefeuille Action.
##' @name update_dur_det_action
##' @docType methods
##' @param x objet de la classe \code{\link{Action}} (decrivant le portefeuille action en detention).
##' @return L'objet \code{x} mis a jour du vieillissement de la duree de detention.
##' @author Prim'Act
##' @export
##' @include Action_class.R

setGeneric(name = "update_dur_det_action", def = function(x) {
    standardGeneric("update_dur_det_action")
})
setMethod(
    f = "update_dur_det_action",
    signature = "Action",
    definition = function(x) {
        # Donnees
        ptf_action <- x@ptf_action
        nom_table <- names(ptf_action)
        dur_det <- which(nom_table == "dur_det")

        if (nrow(ptf_action) != 0L) {
            x@ptf_action$dur_det <- .subset2(ptf_action, dur_det) + 1
        } else {
            warning("[Action : update_dur_det_action] : Le portefeuille action initial est vide.")
        }

        # Output
        return(x)
    }
)
