#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des flux et de pm des autras passifs.
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Extrait les flux et les PM des produits non modelises
##'
##' \code{proj_annee_autres_passifs} est une methode permettant de calculer les PM et les flux sur une annee pour
##' des passif non modelises.
##' Cette methode calcule applique une inflation au frais.
##' @name proj_annee_autres_passifs
##' @docType methods
##' @param an est l'annee de projection.
##' @param x un objet de la classe \code{AutresPassifs} contenant l'ensemble des donnees de passifs non modelises.
##' @param coef_inf un \code{numeric} correpodant au coefficient d'inflation a appliquer sur les frais.
##' @author Prim'Act
##' @return Un \code{data.frame} contenant les flux des passifs de l'annee.
##' @export
##' @include AutresPassifs-class.R
##'
setGeneric(name = "proj_annee_autres_passifs", def = function(an, x, coef_inf) {
    standardGeneric("proj_annee_autres_passifs")
})
setMethod(
    f = "proj_annee_autres_passifs",
    signature = c(an = "integer", x = "AutresPassifs", coef_inf = "numeric"),
    def = function(an, x, coef_inf) {
        # Recuperation du df autres passifs
        mp <- x@mp
        nom_mp <- names(mp)
        num_frais <- which(nom_mp == "frais")
        annee <- .subset2(mp, which(nom_mp == "annee"))

        # Extraction des autres passsifs de l'annee.
        passif_hm_an <- mp[which(annee == an), ]

        # Inflation des frais autres passifs
        passif_hm_an$frais <- .subset2(passif_hm_an, num_frais) * coef_inf

        # output
        return(passif_hm_an)
    }
)
