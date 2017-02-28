#----------------------------------------------------------
# Ce script comprend les methodes de flux et de prestations la classe AutresPassifs
#----------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des flux et de pm des autras passifs. Methode specifique MGP
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Extrait les flux et les PM des produits non modelises
##'
##' \code{proj_annee_autres_passifs} est une methode permettant de calculer les PM et les flux sur une annee des.
##' passif non modelises. Cette methode calcule egalement les frais et leur applique une inflation.
##' @name proj_annee_autres_passifs
##' @docType methods
##' @param an est l'annee de projection.
##' @param x un objet de la classe \code{AutresPassifs} contenant l'ensemble des donnees de passifs non modelises.
##' @param coef_inf un \code{numeric} coorrepodant au coefficient d'inflation a appliquer sur les frais.
##' @author Prim'Act
##' @export
##' @aliases PortPassif
##'

setGeneric(name = "proj_annee_autres_passifs", def = function(an, x, coef_inf)
{standardGeneric("proj_annee_autres_passifs")})
setMethod(
  f = "proj_annee_autres_passifs",
  signature = c(an = "numeric", x = "AutresPassifs", coef_inf = "numeric"),
  def = function(an, x, coef_inf){

    # Recuperation du df autres passifs
    x <- x@mp
    # Extraction des autres passsifs de l'annee.
    passif_hm_an <- x[which(x$annee == an),]
    # Inflation des frais autres passifs
    passif_hm_an$frais <- passif_hm_an$frais * coef_inf

    # output
    return(passif_hm_an)

  }
)



