#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer le rendement annuel de chaque composante du ptf treso
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           revalo_treso
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les valeurs de marches de chaque composante du portefeuille treso.
##'
##' \code{revalo_treso} est une methode permettant de calculer les valeurs de marche.
##' @name revalo_treso
##' @docType methods
##' @param S vecteur de valeur de chaque ligne du ptf en milieu d'annee N (date de calcul des flux).
##' @param S_prev vecteur de valeur de chaque ligne du ptf en milieu d'annee N-1.
##' @return Un vecteur ayant autant d elements que les vecteurs inputs. Chaque element correspondant au rendement annuel d'une lige de tresorerie.
##' @author Prim'Act
##' @export
##' @aliases Treso
##' @include Treso_class.R

# ATTENTION : Rt = R(t,t+1)
# Dans le cas general cette fonction devra donc etre appelee avec Rt_prev force a 0!!!!
setGeneric(name = "revalo_treso", def = function(Rt,Rt_prev){standardGeneric("revalo_treso")})
setMethod(
  f = "revalo_treso",
  signature = c(Rt = "numeric", Rt_prev = "numeric"),
  definition = function(Rt, Rt_prev){
    # Verification des inputs
    if (length(Rt) != length(Rt_prev)) {stop("[Treso : revalo] : Les inputs ont des dimensions distinctes.")}
    if (length(Rt) != 1 | length(Rt_prev) != 1) {stop("[Treso : revalo] : Les inputs doivent etre de dimension 1. \n")}
    # Calcul du vecteur rdt : Prise en compte du fait que les dividendes soient reinvestis ou non
    rdt = (1 + Rt) / (1 + Rt_prev) - 1
    return(rdt)
  }
)

