# 10/03/2017 Guillaume de Kervenoael

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           update_cc_oblig
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour des coupons courus d'un portefeuille obligataire.
##'
##' \code{update_cc_oblig} est une methode permettant de mettre a jour les coupons courus
##'  des composantes d'un portefeuille obligataire.
##' @name update_cc_oblig
##' @docType methods
##' @param x objet de la classe \code{\link{Oblig}} (decrivant le portefeuille obligataire en detention).
##' @param coupon un vecteur de \code{numeric} a assigner a l'objet \code{\link{Oblig}}.
##' @return L'objet \code{x} dont les coupons courus ont ete mis a jour
##' @author Prim'Act
##' @export
##' @include Oblig_class.R

setGeneric(name = "update_cc_oblig", def = function(x, coupon) {
    standardGeneric("update_cc_oblig")
})
setMethod(
    f = "update_cc_oblig",
    signature = c(x = "Oblig", coupon = "numeric"),
    definition = function(x, coupon) {
        # Verification des inputs
        if (nrow(x@ptf_oblig) != length(coupon)) stop("[Oblig : update_oblig] : Les inputs ne sont pas de memes dimensions")

        # Mise a jour coupons courus du PTF
        x@ptf_oblig$cc <- coupon

        # Output
        return(x)
    }
)
