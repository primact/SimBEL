#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_pmvl_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcule les plus et moins-values immobilier.
##'
##' \code{calc_pmvl_immo}  est une methode permettant de calculer les plus et moins-values du portefeuille immobilier.
##' @name calc_pmvl_immo
##' @docType methods
##' @param x objet de la classe \code{\link{Immo}} (decrivant le portefeuille d'immobilier).
##' @return \code{pvl} correspondant a la somme des plus-values latentes immobilier.
##' @return \code{mvl} correspondant a la somme des moins-values latentes immobilier.
##' @author Prim'Act
##' @export
##' @include Immo_class.R

setGeneric(name = "calc_pmvl_immo", def = function(x) {
    standardGeneric("calc_pmvl_immo")
})
setMethod(
    f = "calc_pmvl_immo",
    signature = "Immo",
    definition = function(x) {
        # Donnees
        ptf_immo <- x@ptf_immo
        nom_table <- names(ptf_immo)
        val_marche <- which(nom_table == "val_marche")
        val_nc <- which(nom_table == "val_nc")

        # Plus ou moins value latentes
        pmvl <- .subset2(ptf_immo, val_marche) - .subset2(ptf_immo, val_nc)

        # Calcul des plus et moins values
        pvl <- sum(pmvl * (pmvl > 0))
        mvl <- sum(pmvl * (pmvl <= 0))

        # Output
        return(list(pvl = pvl, mvl = mvl))
    }
)
