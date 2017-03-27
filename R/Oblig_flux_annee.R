#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les fonctions permettant de calculer des flux de la classe Oblig
#--------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_flux_annee
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les flux percus dans l'annee du fait de la detention des obligations du portefeuille obligataire.
##'
##' \code{calc_flux_annee} est une methode permettant de calculer les valeurs nominales de l'ensemble des obligations
##' composant un portefeuille obligataire.
##' @name calc_flux_annee
##' @docType methods
##' @param x un objet de la classe Oblig.
##' @return Une liste composee de deux vecteurs:
##' \describe{
##' \item{\code{tombee_coupon} : }{Chaque element correspond aux tombees de coupon pour l'annee a venir. Ce vecteur a autant d'elements
##' que le portefeuille obligataire d'inputs a de lignes.}
##' \item{\code{tombee_echeance} : }{Chaque element correspond aux tombees d echeances pour l'annee a venir. Ce vecteur a autant d'elements
##' que le portefeuille obligataire d'inputs a de lignes.}
##' }
##' @author Prim'Act
##' @export
##' @aliases Oblig
##' @include Oblig_class.R 

setGeneric(name = "calc_flux_annee", def = function(x){standardGeneric("calc_flux_annee")})
setMethod(
  f = "calc_flux_annee",
  signature = "Oblig",
  definition = function(x){
    tombee_coupon   <- calc_coupon(x)
    tombee_echeance <- calc_nominal(x) * (x["ptf_oblig"][,"mat_res"] <= 1) * 1
    return(list(tombee_coupon = tombee_coupon, tombee_echeance = tombee_echeance))
  }
)
