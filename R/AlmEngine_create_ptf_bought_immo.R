#----------------------------------------------------------------------------------------------------------------------------------------------------
#           create_ptf_bought_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Ajuste les quantites d'immobilier a acheter.
##'
##' \code{create_ptf_bought_immo} est une methode permettant d'ajuster d'un coefficient les quantites
##' d'immobilier a acheter. Cette methode est utilisee pour l'achat de nouveaux titres immobilier.
##' @name create_ptf_bought_immo
##' @docType methods
##' @param x objet de la classe \code{\link{Immo}}, correspondant au portefeuille immobilier de reinvestissement.
##'  Ce portefeuille est unitaire.
##' @param coefficient est un vecteur de type \code{numeric} qui a autant d'elements
##'  que le portefeuille de reinvestissement immo a de lignes. Il correspond au coefficient
##'   a appliquer au portefeuille de reinvestissement immo pour effectuer l'achat desire.
##' @return \code{x} un objet de la classe \code{\link{Immo}} correspondant a une
##' proportion precise du portefeuille de reinvestissement immo.
##' @author Prim'Act
##' @seealso La classe \code{\link{Immo}}.
##' @export
##' @include AlmEngine_class.R Immo_class.R

setGeneric(name = "create_ptf_bought_immo", def = function(x, coefficient) {
    standardGeneric("create_ptf_bought_immo")
})
setMethod(
    f = "create_ptf_bought_immo",
    signature = c(x = "Immo", coefficient = "numeric"),
    definition = function(x, coefficient) {
        # Recuperation du PTF immo
        ptf_immo <- x@ptf_immo

        # Test input
        if (length(coefficient) != nrow(ptf_immo)) stop("[Immo : create_ptf_bought_immo] : Les inputs sont de dimensions distinctes \n")

        # Donnees
        nom_table <- names(ptf_immo)
        val_marche <- which(nom_table == "val_marche")
        val_nc <- which(nom_table == "val_nc")
        val_achat <- which(nom_table == "val_achat")
        nb_unit <- which(nom_table == "nb_unit")

        # Mise a jour des donnes du PTF
        ptf_immo$val_marche <- coefficient * .subset2(ptf_immo, val_marche)
        ptf_immo$val_nc <- coefficient * .subset2(ptf_immo, val_nc)
        ptf_immo$val_achat <- coefficient * .subset2(ptf_immo, val_achat)
        ptf_immo$nb_unit <- coefficient * .subset2(ptf_immo, nb_unit)

        # Mise a jour du PTF
        x@ptf_immo <- ptf_immo

        # Ouput
        return(x)
    }
)
