# 10/03/2017 Guillaume de Kervenoael

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_vm_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante du portefeuille immobilier suite a la vente de tout ou partie de ce portefeuille.
##'
##' \code{update_vm_immo} est une methode permettant de mettre a jour la duree de detention des composantes d'un portefeuille immobilier.
##' @name update_vm_immo
##' @docType methods
##' @param x objet de la classe \code{Immo} (decrivant le portefeuille immobilier en detention).
##' @return L'objet \code{x} mis a jour du vieillissement de la duree de detention.
##' @author Prim'Act
##' @export
##' @aliases Immo
##' @include Immo_class.R


setGeneric(name = "update_vm_immo", def = function(x,vm){standardGeneric("update_vm_immo")})
setMethod(
  f = "update_vm_immo",
  signature = c(x = "Immo", vm = "numeric"),
  definition = function(x,vm){
    # Verification des inputs
    if (nrow(x["ptf_immo"]) != length(vm)) { stop("[Immo : update_vm_immo] Les inputs ne sont pas de memes dimensions")}
    if(sum(vm < 0) > 0) { stop("[Immo : update_vm_immo] :  Le vecteur de VM initialement entre ne peut contenir de valeurs negatives. \n")}
    x["ptf_immo"][,"val_marche"] <- vm
    return(x)
  }
)

