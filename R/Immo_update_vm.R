#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_vm_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour des valeurs de marche d'un portefeuille immobilier.
##'
##' \code{update_vm_immo} est une methode permettant de mettre a jour les valeurs de marche des composantes d'un portefeuille immobilier.
##' @name update_vm_immo
##' @docType methods
##' @param x objet de la classe \code{\link{Immo}} (decrivant le portefeuille immobilier en detention).
##' @param vm un vecteur de \code{numeric} ayant la meme longueur que le portefeuille immobilier a de lignes et correspondant aux nouvelles valeurs
##' de marche du portefeuille immobilier.
##' @return L'objet \code{x} mis a jour.
##' @author Prim'Act
##' @export
##' @include Immo_class.R


setGeneric(name = "update_vm_immo", def = function(x, vm) {
    standardGeneric("update_vm_immo")
})
setMethod(
    f = "update_vm_immo",
    signature = c(x = "Immo", vm = "numeric"),
    definition = function(x, vm) {
        # Donnees
        ptf_immo <- x@ptf_immo

        # Verification des inputs
        if (nrow(ptf_immo) != length(vm)) stop("[Immo : update_vm_immo] : Les inputs ne sont pas de memes dimensions")
        if (sum(vm < 0) > 0) stop("[Immo : update_vm_immo] : Le vecteur de VM initialement entre ne peut contenir de valeurs negatives. \n")

        # Mise a jour de la VM
        x@ptf_immo$val_marche <- vm

        # Output
        return(x)
    }
)
