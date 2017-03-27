#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer les valeurs de marché du ptf Action
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
# Version 1.1 du 30/01/2017. Fait par GK : calc_vm_action renvoi uniquement la nouvelle valeur des VM au lieu de l'objet mis a jour.
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_vm_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les valeurs de marches de chaque composante du portefeuille action.
##'
##' \code{calc_vm_action} est une methode permettant de calculer les valeurs de marche.
##' @name calc_vm_action
##' @docType methods
##' @param x objet de la classe \code{Action} (decrivant le portefeuille d'action).
##' @param rdt vecteur de type \code{numeric} decrivant le rendement de chacune des actions du portefeuille action de l'assureur.
##' Contient autant d'elements que le portefeuille action a de lignes.
##' @return L'objet \code{x} dont les valeurs de marche ont ete mises a jour.
##' @author Prim'Act
##' @export
##' @aliases Action
##' @include Action_class.R

setGeneric(name = "calc_vm_action", def = function(x,rdt){standardGeneric("calc_vm_action")})
setMethod(
  f = "calc_vm_action",
  signature = c(x = "Action", rdt = "numeric"),
  definition = function(x,rdt){
    # Verificateurs
    if(nrow(x["ptf_action"]) == 0) {stop("[Action : calc_vm_action] : Tentative de calcul de VM sur un ptf vide.")}
    if(length(rdt) != nrow(x["ptf_action"])) {stop("[Action : calc_vm_action] : Les inputs ont des dimensions distinctes\n")}

    return(x["ptf_action"][,"val_marche"] * (1+rdt))
  }
)
