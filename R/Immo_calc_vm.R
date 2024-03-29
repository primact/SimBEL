#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_vm_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule les valeurs de marches du portefeuille immobilier.
##'
##' \code{calc_vm_immo} est une methode permettant de calculer les valeurs de marche du portefeuille immobilier.
##' @name calc_vm_immo
##' @docType methods
##' @param x objet de la classe \code{\link{Immo}} (decrivant le portefeuille d'immobilier).
##' @param rdt vecteur de type \code{numeric} decrivant le rendement de chacune des lignes du portefeuille immobilier
##'  de l'assureur.
##' Contient autant d'elements que le portefeuille immobilier a de lignes.
##' @return Les valeurs de marche mises a jour.
##' @author Prim'Act
##' @export
##' @include Immo_class.R

setGeneric(name = "calc_vm_immo", def = function(x, rdt) {
    standardGeneric("calc_vm_immo")
})
setMethod(
    f = "calc_vm_immo",
    signature = c(x = "Immo", rdt = "numeric"),
    definition = function(x, rdt) {
        # Donnees
        ptf_immo <- x@ptf_immo
        nb_immo <- nrow(ptf_immo)
        nom_table <- names(ptf_immo)
        val_marche <- which(nom_table == "val_marche")

        # Verificateurs
        if (nb_immo == 0) stop("[Immo : calc_vm_immo] : Tentative de calcul de VM sur un ptf vide.")
        if (length(rdt) != nb_immo) stop("[Immo : calc_vm_immo] : Les inputs ont des dimensions distinctes\n")

        # Output
        return(.subset2(x@ptf_immo, val_marche) * (1 + rdt))
    }
)
