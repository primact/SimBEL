# 10/03/2017 Guillaume de Kervenoael

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_vm_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de la valeur de marche de chaque composante d'un portefeuille action.
##'
##' \code{update_vm_action} est une methode permettant de mettre a jour la valeur de marche des composantes d'un portefeuille Action.
##' @name update_vm_action
##' @docType methods
##' @param x objet de la classe \code{Action} (decrivant le portefeuille action en detention).
##' @param vm un vecteur de \code{numeric} ayant la meme longueur que le portefeuille action a de lignes et correspondant aux nouvelles valeurs de marche du portefeuille action.
##' @return L'objet \code{x} mis a jour du vieillissement de la duree de detention.
##' @author Prim'Act
##' @export
##' @aliases Action
##' @include Action_class.R


setGeneric(name = "update_vm_action", def = function(x,vm){standardGeneric("update_vm_action")})
setMethod(
  f = "update_vm_action",
  signature = c(x = "Action", vm = "numeric"),
  definition = function(x,vm){
    # Verification des inputs
    if (nrow(x["ptf_action"]) != length(vm)) { stop("[Action : update_vm_action] Les inputs ne sont pas de memes dimensions")}
    if(sum(vm < 0) > 0) { stop("[Action : update_vm_action] :  Le vecteur de VM initialement entre ne peut contenir de valeurs negatives. \n")}
    x["ptf_action"][,"val_marche"] <- vm
    return(x)
  }
)

