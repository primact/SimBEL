#----------------------------------------------------------------------------------------------------------------------------------------------------
# calc_proba_deces : Methode de calcul du vecteur des probabilites de deces.
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule le vecteur des probabilites cumulees de deces
##'
##' \code{calc_proba_deces} est une methode permettant de calculer le vecteur des probabilites cumulees de deces.
##' @name calc_proba_deces
##' @docType methods
##' @param table_mort un objet de la classe \code{\link{ParamTableMort}} contenant la table de mortalite.
##' @param age une valeur \code{integer} correspondant a l'age.
##' @param gen une valeur \code{integer} correspondant a la generation.
##' @param n_periodes une valeur \code{integer} correspondant a la borne de sommation.
##' @return La valeur du taux de deces calcule.
##' @author Prim'Act
##' @include ParamTableMort-class.R
##' @export

setGeneric("calc_proba_deces", function(table_mort, age, gen, n_periodes) {
    standardGeneric("calc_proba_deces")
})
setMethod(
    f = "calc_proba_deces",
    signature = c(table_mort = "ParamTableMort", age = "integer", gen = "integer", n_periodes = "integer"),
    def = function(table_mort, age, gen, n_periodes) {
        # Calcul du vecteur de probas
        vecteur_deces <- 1 - calc_proba_survie(table_mort, age, gen, n_periodes)

        # Output
        return(vecteur_deces)
    }
)
