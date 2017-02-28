#----------------------------------------------------------
# Ce script comprend les methodes de la classe EpEuroInd
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 22/01/2017. Fait par QG : initialisation
# Version 1.1 du 23/01/2017. Fait par QG : ajout d autre taux de revalorisation
#----------------------------------------------------------


##' Calcul des differents taux  revalorisation minimum d'un model point.
##'
##' \code{calc_tx_min} est une methode permettant de calculer le taux de revalorisation minimum sur
##'  une annee.
##' @name calc_tx_min
##' @docType methods
##' @param x un objet de la classe \code{EpEuroInd} contenant les model points epargne euros.
##' @param an un numeric represant l'annee de projection courante
##' @return Une data.frame contenant :
##' \describe{
##' \item{\code{tx_tech_an} : }{un vecteur contenant les taux de technique de l'annee}
##' \item{\code{tx_tech_an} : }{un vecteur contenant les taux de technique de l'annee sur base semestrielle}
##' \item{\code{tx_an} : }{un vecteur contenant les taux de revalorisation minimum de l'annee}
##' \item{\code{tx_se} : }{un vecteur contenant les taux de revalorisation minimum de l'annee exprimes en semestriel}
##' }
##'
##'
##' @author Prim'Act
##' @export
##' @aliases EpEuroInd
##'

setGeneric(name = "calc_tx_min", def = function(x, an){standardGeneric("calc_tx_min")})
#--------------------------------------------------------

setMethod(
  f = "calc_tx_min",
  signature = c(x = "EpEuroInd", an ="numeric"),
  def = function(x, an){

    nb_mp <- nrow(x@mp)
    # Calcul des indicatrice de versement du taux technique et du tmg
    ind_tx_tech <- rep(0, nb_mp)
    ind_tx_tech[an <= x@mp$terme_tx_tech] <- 1 # indicatrice taux technique pour l'annee en cours
    ind_tmg <- rep(0, nb_mp)
    ind_tmg[an <= x@mp$terme_tmg] <- 1 # indicatrice taux technique pour l'annee en cours

    # calcul du taux technique
    tx_tech <- pmax(0, x@mp$tx_tech * ind_tx_tech) # taux technique
    tx_tech_se <- taux_period(tx_tech, period = "se") # taux semestriel

    # Calcul du taux minimum
    tx_min <- pmax(0, x@mp$tx_tech * ind_tx_tech, x@mp$tmg * ind_tmg) # taux annuel minimum
    tx_min_se <- taux_period(tx_min, period = "se") # taux semestriel

    # Output
    return(list(
      tx_tech_an = tx_tech,
      tx_tech_se = tx_tech_se,
      tx_an = tx_min,
      tx_se = tx_min_se))

  }
)
