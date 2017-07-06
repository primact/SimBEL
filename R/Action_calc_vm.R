#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_vm_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule les valeurs de marches du portefeuille action.
##'
##' \code{calc_vm_action} est une methode permettant de calculer les valeurs de marche du portefeuille action.
##' @name calc_vm_action
##' @docType methods
##' @param x objet de la classe \code{\link{Action}} (decrivant le portefeuille d'action).
##' @param rdt vecteur de type \code{numeric} decrivant le rendement de chacune des lignes du portefeuille action
##'  de l'assureur.
##' Contient autant d'elements que le portefeuille action a de lignes.
##' @return Les valeurs de marche mises a jour.
##' @author Prim'Act
##' @export
##' @include Action_class.R

setGeneric(name = "calc_vm_action", def = function(x,rdt){standardGeneric("calc_vm_action")})
setMethod(
    f = "calc_vm_action",
    signature = c(x = "Action", rdt = "numeric"),
    definition = function(x, rdt){

        # Numeros colonnes donnees
        ptf_action <- x@ptf_action
        nb_action  <- nrow(ptf_action)
        nom_table  <- names(ptf_action)
        val_marche <- which(nom_table == "val_marche")

        # Verificateurs
        if(nb_action == 0L)            stop("[Action : calc_vm_action] : Tentative de calcul de VM sur un ptf vide.")
        if(length(rdt) != nb_action)   stop("[Action : calc_vm_action] : Les inputs ont des dimensions distinctes\n")

        # Output
        return(.subset2(ptf_action, val_marche) * (1 + rdt))
    }
)
