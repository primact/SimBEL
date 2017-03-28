
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_nominal
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le nominal des obligations constituant le portefeuille obligataire.
##'
##' \code{calc_nominal} est une methode permettant de calculer les valeurs de nominal de l'ensemble des obligations
##' composant un portefeuille obligataire.
##' @name calc_nominal
##' @docType methods
##' @param x un objet de la classe \code{\link{Oblig}}.
##' @return Un vecteur dont chaque element correspond a la valeur du nominal de l'obligation consideree : parite * nominal * nb_unit.
##' Le vecteur renvoye a autant d'elements que le portefeuille obligataire en input a de lignes.
##' @author Prim'Act
##' @export
##' @include Oblig_class.R

setGeneric(name = "calc_nominal", def = function(x){standardGeneric("calc_nominal")})
setMethod(
    f = "calc_nominal",
    signature = c(x = "Oblig"),
    definition = function(x){
        nom_table <- names(x@ptf_oblig)
        par       <- which(nom_table == "par")
        nominal   <- which(nom_table == "nominal")
        nb_unit   <- which(nom_table == "nb_unit")

        return(.subset2(x@ptf_oblig, par) * .subset2(x@ptf_oblig, nominal) * .subset2(x@ptf_oblig,nb_unit))
    }
)
