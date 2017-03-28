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
##' @return L'objet \code{x} dont les valeurs de marche ont ete mises a jour.
##' @author Prim'Act
##' @export
##' @include Action_class.R

setGeneric(name = "calc_vm_action", def = function(x,rdt){standardGeneric("calc_vm_action")})
setMethod(
  f = "calc_vm_action",
  signature = c(x = "Action", rdt = "numeric"),
  definition = function(x, rdt){
    # Verificateurs
    if(nrow(x@ptf_action) == 0) {stop("[Action : calc_vm_action] : Tentative de calcul de VM sur un ptf vide.")}
    if(length(rdt) != nrow(x@ptf_action)) {stop("[Action : calc_vm_action] : Les inputs ont des dimensions distinctes\n")}

    nom_table  <- names(x@ptf_action)
    val_marche <- which(nom_table == "val_marche")
    return(.subset2(x@ptf_action, val_marche) * (1 + rdt))
  }
)
