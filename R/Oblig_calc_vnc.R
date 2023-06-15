#----------------------------------------------------------------------------------------------------------------------------------------------------
#           calc_vnc
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les valeurs nettes comptables de chaque composante portefeuille obligataires.
##'
##' \code{calc_vnc} est une methode permettant de calculer les valeurs de marche.
##' @name calc_vnc
##' @docType methods
##' @param x objet de la classe \code{\link{Oblig}} (decrivant le portefeuille d'obligation).
##' @param sd_unitaire vecteur de type \code{numeric} decrivant la surcote decote de chacune des lignes d'obligation du portefeuille obligation de
##' l'assureur. Contient autant d'elements que le portefeuille a de lignes.
##' @return L'objet \code{x} dont les valeurs nettes comptables ont ete mises a jour.
##' @author Prim'Act
##' @export
##' @include Oblig_class.R

setGeneric(name = "calc_vnc", def = function(x, sd_unitaire) {
    standardGeneric("calc_vnc")
})
setMethod(
    f = "calc_vnc",
    signature = c("Oblig", "numeric"),
    definition = function(x, sd_unitaire) {
        # Recuperation du PTF oblig
        ptf_oblig <- x@ptf_oblig
        nb_oblig <- nrow(ptf_oblig)

        # Tests
        if (nb_oblig == 0L) stop("[Oblig : calc_vnc] : Portefeuille obligataire vide")
        if (length(sd_unitaire) != nb_oblig) stop("[[Oblig : calc_vnc] : Inputs de mauvaise dimension")

        # Donnees
        nom_table <- names(ptf_oblig)
        nb_unit <- which(nom_table == "nb_unit")
        val_nc <- which(nom_table == "val_nc")

        # Output
        return(.subset2(ptf_oblig, val_nc) + sd_unitaire * .subset2(ptf_oblig, nb_unit))
    }
)
