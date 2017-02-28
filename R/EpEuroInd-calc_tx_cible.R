#----------------------------------------------------------
# Ce script comprend les methodes de la classe EpEuroInd
#----------------------------------------------------------
# Suivi version
# Version 1.0 du 31/01/2017. Fait par MT : initialisation
#----------------------------------------------------------


##' Calcul des differents taux cible pour chaque model point de la classe EpEuroInd
##'
##' \code{calc_tx_cible} est une methode permettant de calculer un taux cible
##'   pour chaque model point de la classe EpEuroInd.
##' @name calc_tx_cible
##' @docType methods
##' @param epi un objet de la classe \code{EpEuroInd} contenant les model points epargne euros.
##' @param ht un objet de la classe \code{HypTech} contenant differentes tables en entree.
##' @param list_rd une liste contenant les rendements de reference.
##' @return Une liste contenant le taux cible annuel et le taux cible semestriel par model points.
##' @author Prim'Act
##' @export
##' @aliases EpEuroInd

setGeneric(name = "calc_tx_cible",
           def = function(epi, ht, list_rd)
          {standardGeneric("calc_tx_cible")})
#--------------------------------------------------------

setMethod(
  f = "calc_tx_cible",
  signature = c(epi = "EpEuroInd", ht = "HypTech", list_rd = "list"),
  def = function(epi, ht, list_rd){

    # Nom de ligne
    nb_mp <- nrow(epi@mp)

    # Gestion des noms de colonnes du data.frame de donnnees
    nom_table = which(names(epi@mp) == "tx_cible")
    tx_cible_prec = which(names(epi@mp) == "tx_cible_prec")

    # Fonction d'extraction du taux cible
    calc_tx_cible_mp <- function(i) {return(
      get_comport(ht,
        as.character(.subset2(epi@mp, nom_table)[i]),
        list_rd,
        .subset2(epi@mp, tx_cible_prec)[i])
      )
    }

    # Calcul du taux cible annuel et semestriel
    tx_cible_an <- sapply(1:nb_mp, calc_tx_cible_mp)
    tx_cible_se <- taux_period(tx_cible_an, "se")

  return(list(tx_cible_an = tx_cible_an, tx_cible_se = tx_cible_se))

  }
)
