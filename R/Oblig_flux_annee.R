#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_flux_annee
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule les flux percus dans l'annee du portefeuille obligataire.
##'
##' \code{calc_flux_annee} est une methode permettant de calculer les tombees de coupons et les echeances
##'  l'ensemble des obligations composant un portefeuille obligataire.
##' @name calc_flux_annee
##' @docType methods
##' @param x un objet de la classe \code{\link{Oblig}}.
##' @return \code{tombee_coupon} un vecteur correspondant aux tombees de coupon dans l'annee. Ce vecteur a autant d'elements
##' que le portefeuille obligataire d'inputs a de lignes.
##' @return \code{tombee_echeance} un vecteur correspondant aux tombees d echeances dans l'annee. Ce vecteur a autant d'elements
##' que le portefeuille obligataire d'inputs a de lignes.
##' @author Prim'Act
##' @export
##' @include Oblig_class.R

setGeneric(name = "calc_flux_annee", def = function(x){standardGeneric("calc_flux_annee")})
setMethod(
  f = "calc_flux_annee",
  signature = "Oblig",
  definition = function(x){
    tombee_coupon   <- calc_coupon(x)
    tombee_echeance <- calc_nominal(x) * (.subset2(x@ptf_oblig, which(names(x@ptf_oblig) == "mat_res")) <= 1) * 1
    return(list(tombee_coupon = tombee_coupon, tombee_echeance = tombee_echeance))
  }
)
