#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer les valeurs de marché du ptf Immo
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
# Version 1.1 du 30/01/2017. Fait par GK : calc_vm_immo renvoi uniquement la nouvelle valeur des VM au lieu de l'objet mis a jour.
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_vm_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les valeurs de marches de chaque composante du portefeuille immo.
##'
##' \code{calc_vm_immo} est une methode permettant de calculer les valeurs de marche.
##' @name calc_vm_immo
##' @docType methods
##' @param x objet de la classe Immo (decrivant le portefeuille d'immo).
##' @param rdt vecteur decrivant le rendement de chacun des biens immo du ptf.
##' Contient autant d'elements que le portefeuille a de lignes.
##' @return L'objet x dont les valeurs de marche ont ete mises a jour.
##' @author Prim'Act
##' @export
##' @aliases Treso

setGeneric(name = "calc_vm_immo", def = function(x,rdt){standardGeneric("calc_vm_immo")})
setMethod(
  f = "calc_vm_immo",
  signature = c(x = "Immo", rdt = "numeric"),
  definition = function(x,rdt){
    # Verificateurs
    if(nrow(x["ptf_immo"]) == 0) {stop("[Immo : calc_vm_immo] : Tentative de calcul de VM sur un ptf vide.")}
    if(length(rdt) != nrow(x["ptf_immo"])) {stop("[Immo : calc_vm_immo] : Les inputs ont des dimensions distinctes\n")}
    #x["ptf_immo"][,"val_marche"] <- x["ptf_immo"][,"val_marche"] * (1+rdt) : version 1.0

    return(x["ptf_immo"][,"val_marche"] * (1+rdt))
  }
)



setGeneric(name = "calc_pmvl_immo", def = function(x){standardGeneric("calc_pmvl_immo")})
setMethod(
  f = "calc_pmvl_immo",
  signature = "Immo",
  definition = function(x){
    pvl <- sum((x["ptf_immo"][,"val_marche"] - x["ptf_immo"][,"val_nc"])*(x["ptf_immo"][,"val_marche"] > x["ptf_immo"][,"val_nc"]))
    mvl <- sum((x["ptf_immo"][,"val_marche"] - x["ptf_immo"][,"val_nc"])*(x["ptf_immo"][,"val_marche"] < x["ptf_immo"][,"val_nc"]))
    return(list(pvl = pvl, mvl = mvl))
  }
)
