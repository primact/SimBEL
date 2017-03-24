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
##' @aliases AlmEngine
##' @include AlmEngine_class.R Immo_class.R

setGeneric(name = "create_ptf_bought_immo", def = function(x, coefficient){standardGeneric("create_ptf_bought_immo")})
setMethod(
    f = "create_ptf_bought_immo",
    signature = c(x = "Immo", coefficient = "numeric"),
    definition = function(x, coefficient){
        if (length(coefficient) != nrow(x@ptf_immo)){stop("[Immo : create_ptf_bought_immo] : Les inputs sont de dimensions distinctes \n")}
        x@ptf_immo$val_marche <- coefficient * x@ptf_immo$val_marche
        x@ptf_immo$val_nc     <- coefficient * x@ptf_immo$val_nc
        x@ptf_immo$val_achat  <- coefficient * x@ptf_immo$val_achat
        x@ptf_immo$nb_unit    <- coefficient * x@ptf_immo$nb_unit
        return(x)
    }
)
