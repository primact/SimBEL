#----------------------------------------------------------
# Ce script comprend les methodes de la classe EpEuroInd
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul du taux cible pour des contrats epargne en euros.
##'
##' \code{calc_tx_cible} est une methode permettant d'evaluer le taux de revalorisation cible
##'   de chaque model point.
##' @name calc_tx_cible
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} contenant les model points epargne euros.
##' @param ht un objet de la classe \code{\link{HypTech}} contenant differentes lois de comportement.
##' @param list_rd une liste contenant les rendements de reference. Le format de cette liste est :
##' \describe{
##' \item{le taux de rendement obligataire}{}
##' \item{le taux de rendement de l'indice action de reference}{}
##' \item{le taux de rendement de l'indice immobilier de reference}{}
##' \item{le taux de rendement de l'indice tresorerie de reference}{}
##' }
##' @return \code{tx_cible_an} : un vecteur contenant les taux cible de l'annee
##' @return \code{tx_cible_se} : un vecteur contenant les taux cible de l'annee sur base semestrielle
##' @note Pour les besoins des calculs a mi-annee, des taux semestriels sont produits.
##' @author Prim'Act
##' @seealso La recuperation des taux cibles calcules : \code{\link{get_comport}}.
##' @export
##' @aliases EpEuroInd
##' @include EpEuroInd-class.R HypTech-class.R

setGeneric(name = "calc_tx_cible",
           def = function(x, ht, list_rd)
          {standardGeneric("calc_tx_cible")})
#--------------------------------------------------------

setMethod(
  f = "calc_tx_cible",
  signature = c(x = "EpEuroInd", ht = "HypTech", list_rd = "list"),
  def = function(x, ht, list_rd){

    # Nom de ligne
    nb_mp <- nrow(x@mp)

    # Gestion des noms de colonnes du data.frame de donnnees
    nom_table = which(names(x@mp) == "tx_cible")
    tx_cible_prec = which(names(x@mp) == "tx_cible_prec")

    # Fonction d'extraction du taux cible
    calc_tx_cible_mp <- function(i) {return(
      get_comport(ht,
        as.character(.subset2(x@mp, nom_table)[i]),
        list_rd,
        .subset2(x@mp, tx_cible_prec)[i])
      )
    }

    # Calcul du taux cible annuel et semestriel
    tx_cible_an <- sapply(1:nb_mp, calc_tx_cible_mp)
    tx_cible_se <- taux_period(tx_cible_an, "se")

  return(list(tx_cible_an = tx_cible_an, tx_cible_se = tx_cible_se))

  }
)
