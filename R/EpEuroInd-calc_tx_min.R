#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des taux de revalorisation minimum d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' Calcul le taux de revalorisation contractuel minimum pour des contrats epargne en euros.
##'
##' \code{calc_tx_min} est une methode permettant de calculer les taux de revalorisation minimum
##'   sur une periode. La revalorisation minimum est le maximum entre le taux technique et
##'   le taux minimim garanti (TMG) du contrat.
##' @name calc_tx_min
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} contenant les model points epargne euros.
##' @param an un \code{numeric} representant l'annee de projection courante.
##' @return \code{tx_tech_an} : un vecteur contenant les taux de technique de l'annee
##' @return \code{tx_tech_se} : un vecteur contenant les taux de technique de l'annee sur base semestrielle
##' @return \code{tx_an} : un vecteur contenant les taux de revalorisation minimum de l'annee
##' @return \code{x_se} : un vecteur contenant les taux de revalorisation minimum de l'annee exprimes en semestriel.
##' @note Pour les besoins des calculs a mi-annee, des taux semestriels sont produits.
##' @author Prim'Act
##' @export
##' @include EpEuroInd-class.R

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
