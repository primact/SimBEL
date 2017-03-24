
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Calc_coupon
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul le coupon des models points constituant le portefeuille obligataire.
##'
##' \code{calc_coupon} est une methode permettant de calculer les valeurs de coupon de l'ensemble des obligations
##' composant un portefeuille obligataire.
##' @name calc_coupon
##' @docType methods
##' @param x un objet de la classe Oblig.
##' @return Un vecteur dont chaque element correspond a la valeur du coupon de l'obligation consideree : tx_coupon * parite * nominal * nb_unit.
##' Le vecteur renvoye a autant d'elements que le portefeuille obligataire en input a de lignes.
##' @author Prim'Act
##' @export
##' @aliases Oblig
##' @include Oblig_class.R

setGeneric(name = "calc_coupon", def = function(x){standardGeneric("calc_coupon")})
setMethod(
    f = "calc_coupon",
    signature ="Oblig",
    definition = function(x){
        # Tx_coupon * Parite * Nominal
        return(x["ptf_oblig"][,"tx_coupon"] * x["ptf_oblig"][,"par"] * x["ptf_oblig"][,"nominal"] * x["ptf_oblig"][,"nb_unit"])
    }
)