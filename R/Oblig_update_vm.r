# 10/03/2017 Guillaume de Kervenoael

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_vm_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour des valeurs de marche d'un portefeuille obligataire.
##'
##' \code{update_vm_oblig} est une methode permettant de mettre a jour les valeurs de marche des composantes d'un portefeuille obligataire.
##' @name update_vm_oblig
##' @docType methods
##' @param x objet de la classe \code{Oblig} (decrivant le portefeuille obligataire en detention).
##' @param vm un vecteur de \code{numeric} a assigner a l'objet \code{Obligation}. 
##' @return L'objet \code{x} dont les valeurs de marche ont ete mises a jour.
##' @author Prim'Act
##' @export
##' @aliases Oblig
##' @include Oblig_class.R

setGeneric(name = "update_vm_oblig", def = function(x,vm){standardGeneric("update_vm_oblig")})
setMethod(
    f = "update_vm_oblig",
    signature = c(x = "Oblig", vm = "numeric"),
    definition = function(x, vm){
        # Verification des inputs
        if (nrow(x["ptf_oblig"]) != length(vm)) { stop("[Oblig : update_oblig] Les inputs ne sont pas de memes dimensions")}
        if(sum(vm < 0) > 0) { stop("[Oblig : update_vm_oblig] :  Le vecteur de VM initialement entre ne peut contenir de valeurs negatives. \n")}
        x["ptf_oblig"][,"val_marche"] <- vm
        return(x)
    }
)