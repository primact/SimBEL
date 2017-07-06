#----------------------------------------------------------------------------------------------------------------------------------------------------
#           resultat_fin
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le resultat financier.
##'
##' \code{calc_resultat_fin} est une methode permettant de calculer le resultat financier du portefeuille.
##' @name calc_resultat_fin
##' @docType methods
##' @param revenu est un objet de type \code{numeric}, qui fournit les revenus du portefeuille financier.
##' @param produit est un objet de type \code{numeric}, qui fournit le produit (ou la perte) des cessions.
##' @param frais_fin est un objet de type \code{numeric}, qui fournit le montant des frais financiers.
##' @param var_rc est un objet de type\code{numeric}, donnant la variation de la reserve de capitalisation.
##' @return La valeur du result financier.
##' @author Prim'Act
##' @export
##' @include PortFin_class.R

setGeneric(name = "calc_resultat_fin", def = function(revenu, produit, frais_fin, var_rc){standardGeneric("calc_resultat_fin")})
setMethod(
    f = "calc_resultat_fin",
    signature = c(revenu = "numeric", produit = "numeric", frais_fin = "numeric", var_rc = "numeric"),
    definition = function(revenu, produit, frais_fin, var_rc){

        # Calcul du resultat financier
        res <- revenu + produit - frais_fin - var_rc

        # Output
        return(res)
    }
)

