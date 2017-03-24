#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer le rendement annuel de chaque composante du ptf Action
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
# 10/03/2017 : GK mise en commentaire
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           revalo_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les valeurs de marches de chaque composante du portefeuille action.
##'
##' \code{revalo_action} est une methode permettant de calculer les valeurs de marche.
##' @name revalo_action
##' @docType methods
##' @param x objet de la classe \code{Action} (decrivant le portefeuille d'action).
##' @param S vecteur \code{numeric} de valeur de chaque stock du ptf en milieu d'annee N (date de versement des dividendes)
##' @param S_prev vecteur \code{numeric}  de valeur de chaque stock du ptf en milieu d'annee N-1.
##' @return Un data frame compose de deux colonnes et autant de lignes que le portefeuille action a de lignes.
##' La premiere colonne decrit de le rendement annuel de chacune des actions composants le portefeuille action.
##' La seconde colonne decrit les dividendes annuelles percues au titre de chacune des actions composants le portefeuille action.
##' @author Prim'Act
##' @export
##' @aliases Action
##' @include Action_class.R

setGeneric(name = "revalo_action", def = function(x,S,S_prev){standardGeneric("revalo_action")})
setMethod(
  f = "revalo_action",
  signature = c(x = "Action", S = "numeric", S_prev = "numeric"),
  definition = function(x,S,S_prev){
    # Verification des inputs
    if (length(S) != length(S_prev) | nrow(x["ptf_action"]) != length(S)) {stop("[Action : revalo] : Les inputs ont des dimensions distinctes.")}
    # Calcul du vecteur rdt : Prise en compte du fait que les dividendes soient reinvestis ou non
    rdt = (S/S_prev) / (1 + x["ptf_action"][,"div"] * x["ptf_action"][,"ind_invest"]) - 1
    # Calcul des dividendes verses en tresorerie en milieu d'annee
    div = x["ptf_action"][,"val_marche"] * (1 + rdt)^0.5 * x["ptf_action"][,"div"]
    return(data.frame(rdt,div))
  }
)

