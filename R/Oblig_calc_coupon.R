#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Calc_coupon
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule les coupons d'un portefeuille obligataire.
##'
##' \code{calc_coupon} est une methode permettant de calculer les valeurs de coupon de l'ensemble des obligations
##' composant un portefeuille obligataire.
##' @name calc_coupon
##' @docType methods
##' @param x un objet de la classe \code{\link{Oblig}}, dont on souhaite calculer le coupon
##'  annuel pour chacune de ses composantes.
##' @return un vecteur dont chaque element correspond a la valeur du coupon de l'obligation consideree
##'  , i.e tx_coupon * parite * nominal * nb_unit.
##' Le vecteur renvoye a autant d'elements que le portefeuille obligataire en input a de lignes.
##' @author Prim'Act
##' @export
##' @include Oblig_class.R

setGeneric(name = "calc_coupon", def = function(x) {
    standardGeneric("calc_coupon")
})
setMethod(
    f = "calc_coupon",
    signature = "Oblig",
    definition = function(x) {
        # Donnees
        ptf_oblig <- x@ptf_oblig
        nom_table <- names(ptf_oblig)
        tx_coupon <- which(nom_table == "tx_coupon")
        par <- which(nom_table == "par")
        nominal <- which(nom_table == "nominal")
        nb_unit <- which(nom_table == "nb_unit")

        return(.subset2(ptf_oblig, tx_coupon) * .subset2(ptf_oblig, par) * .subset2(ptf_oblig, nominal) * .subset2(ptf_oblig, nb_unit))
    }
)
