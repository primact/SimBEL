#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_dur_det_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour des durees de detention d'un portefeuille immobilier.
##'
##' \code{update_dur_det_immo} est une methode permettant de mettre a jour la duree de detention des composantes d'un portefeuille immobilier.
##' @name update_dur_det_immo
##' @docType methods
##' @param x objet de la classe \code{\link{Immo}} (decrivant le portefeuille immo en detention).
##' @return L'objet \code{x} mis a jour du vieillissement de la duree de detention.
##' @author Prim'Act
##' @export
##' @include Immo_class.R

setGeneric(name = "update_dur_det_immo", def = function(x) {
    standardGeneric("update_dur_det_immo")
})
setMethod(
    f = "update_dur_det_immo",
    signature = "Immo",
    definition = function(x) {
        # Donnees
        ptf_immo <- x@ptf_immo
        nom_table <- names(ptf_immo)
        dur_det <- which(nom_table == "dur_det")

        if (nrow(ptf_immo) != 0L) {
            x@ptf_immo$dur_det <- .subset2(ptf_immo, dur_det) + 1
        } else {
            warning("[Immo : update_dur_det_immo] : Le portefeuille immo initial est vide.")
        }

        # Output
        return(x)
    }
)
