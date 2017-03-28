
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_sur_dec
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les surcotes/decotes de chaque composante d'un portefeuille obligataire.
##'
##' \code{calc_sur_dec} est une methode permettant de calculer les surcotes/decotes de chaque composante d'un portefeuille obligataire.
##' @name calc_sur_dec
##' @docType methods
##' @param x objet de la classe \code{\link{Oblig}} (decrivant le portefeuille obligataire).
##' @return Un vecteur contenant les surcotes decotes.
##' @author Prim'Act
##' @export
##' @include Oblig_class.R

setGeneric(name = "calc_sur_dec", def = function(x){standardGeneric("calc_sur_dec")})
setMethod(
    f = "calc_sur_dec",
    signature = "Oblig",
    definition = function(x){

        nom_table <- names(x@ptf_oblig)
        nb_unit   <- which(nom_table == "nb_unit")
        mat_res   <- which(nom_table == "mat_res")
        val_nc    <- which(nom_table == "val_nc")

        if(nrow(x@ptf_oblig) == 0) {stop("[Oblig:calc_sur_dec_vnc] : Portefeuille obligataire vide")}
        nominal           <- calc_nominal(x) / .subset2(x@ptf_oblig, nb_unit)
        vnc               <- .subset2(x@ptf_oblig, val_nc) / .subset2(x@ptf_oblig, nb_unit)
        maturite_initiale <- .subset2(x@ptf_oblig, mat_res)
        surcote_decote    <- (nominal - vnc) / maturite_initiale
        return(surcote_decote)
    }
)
