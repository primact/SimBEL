#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer les valeurs de marché du ptf treso
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 24/01/2017. Fait par GK : initialisation
# Version 1.1 du 30/01/2017. Fait par GK : calc_vm_treso renvoi un data frame dont la nouvelle valeur des VM au lieu de l'objet mis a jour.
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_vm_treso
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les valeurs de marches de chaque composante du portefeuille treso.
##'
##' \code{calc_vm_treso} est une methode permettant de calculer les valeurs de marche de chaque ligne du portefeuille treso.
##' @name calc_vm_treso
##' @docType methods
##' @param x objet de la classe treso (decrivant le portefeuille de treso).
##' @param rdt vecteur decrivant le rendement de chacune des lignes treso du ptf.
##' Contient autant d'elements que le portefeuille a de lignes.
##' @param flux_milieu vecteur decrivant les flux ( percus)entrants : positif, sortants : negatifs) en milieu d'annee, ventiles selon chacune des lignes de cash.
##' @param flux_fin vecteur decrivant les flux (entrants : positifs, sortants : negatifs) en fin d'annee, ventiles selon chacune des lignes de cash.
##' @return L'objet x dont les valeurs de marche ont ete mises a jour.
##' @author Prim'Act
##' @export
##' @aliases Treso
##' @include Treso_class.R


setGeneric(name = "calc_vm_treso", def = function(x,rdt,flux_milieu,flux_fin){standardGeneric("calc_vm_treso")})
setMethod(
  f = "calc_vm_treso",
  signature = c(x = "Treso", rdt = "numeric", flux_milieu = "numeric", flux_fin = "numeric"),
  definition = function(x,rdt,flux_milieu, flux_fin){
    # Verification des inputs
    if(nrow(x["ptf_treso"]) == 0) {stop("[Treso : calc_vm_treso] : Tentative de calcul de VM sur un portfeuille de treso vide. \n")}
    if(length(rdt) != nrow(x["ptf_treso"]) | length(rdt) != length(flux_milieu) | length(flux_milieu) != length(flux_fin)) {stop("[Treso : revalo] : Les inputs ont des dimensions distinctes\n")}
      if(length(rdt) > 1 | length(flux_milieu) > 1 | length(flux_fin) > 1) {stop("[Treso : revalo] : Les inputs doivent etre de dimension 1. \n")}
      
    val_marche <- x["ptf_treso"][,"val_marche"] * (1 + rdt) + flux_milieu * (1 + rdt)^0.5 + flux_fin

    return(val_marche)
  }
)
