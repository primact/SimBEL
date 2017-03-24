#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer le rendement annuel de chaque composante du ptf Immo
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
# 10/03/2017 : GK mise en commentaire
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           revalo_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les valeurs de marches de chaque composante du portefeuille immobilier.
##'
##' \code{revalo_immo} est une methode permettant de calculer les valeurs de marche.
##' @name revalo_immo
##' @docType methods
##' @param x objet de la classe \code{Immo} (decrivant le portefeuille d'immobilier).
##' @param S vecteur \code{numeric} de valeur de chaque stock du ptf en milieu d'annee N (date de versement des dividendes)
##' @param S_prev vecteur \code{numeric}  de valeur de chaque stock du ptf en milieu d'annee N-1.
##' @return Un data frame compose de deux colonnes et autant de lignes que le portefeuille immobilier a de lignes.
##' La premiere colonne decrit de le rendement annuel de chacune des lignes d'immobilier composants le portefeuille immobilier.
##' La seconde colonne decrit les dividendes annuelles percues au titre de chacune des lignes d'immobilier composants le portefeuille immobilier.
##' @author Prim'Act
##' @export
##' @aliases Immo
##' @include Immo_class.R

setGeneric(name = "revalo_immo", def = function(x,S,S_prev){standardGeneric("revalo_immo")})
setMethod(
  f = "revalo_immo",
  signature = c(x = "Immo", S = "numeric", S_prev = "numeric"),
  definition = function(x,S,S_prev){
    # Verification des inputs
    if (length(S) != length(S_prev) | nrow(x["ptf_immo"]) != length(S)) {stop("[Immo : revalo] : Les inputs ont des dimensions distinctes.")}
    # Calcul du vecteur rdt : Prise en compte du fait que les loyers soient reinvestis ou non
    rdt = (S/S_prev) / (1 + x["ptf_immo"][,"loyer"] * x["ptf_immo"][,"ind_invest"]) - 1
    # Calcul des loyers verses en tresorerie en milieu d'annee
    loyer = x["ptf_immo"][,"val_marche"] * (1 + rdt)^0.5 * x["ptf_immo"][,"loyer"]
    return(data.frame(rdt,loyer))
  }
)
