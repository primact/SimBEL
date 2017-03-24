##' Ajuste les quantites d'obligations a acheter.
##'
##' \code{create_ptf_bought_oblig} est une methode permettant d'ajuster d'un coefficient les quantites
##' d'obligations a acheter. Cette methode est utilisee pour l'achat de nouveaux titres obligataires.
##' @name create_ptf_bought_oblig
##' @docType methods
##' @param x objet de la classe \code{\link{Oblig}}, correspondant au portefeuille
##'  obligataire de reinvestissement. Ce portefeuille est unitaire.
##' @param coefficient est un vecteur de type \code{numeric} qui a autant d'elements
##'  que le portefeuille de reinvestissement obligataire a de lignes. Il correspond au coefficient
##'   a appliquer au portefeuille de reinvestissement obligataire pour effectuer l'achat desire.
##' @return \code{x} un objet de la classe \code{\link{Oblig}} correspondant a une proportion
##'  precise du portefeuille de reinvestissement obligataire.
##' @author Prim'Act
##' @seealso La classe \code{\link{Oblig}}.
##' @export
##' @aliases AlmEngine
##' @include AlmEngine_class.R Oblig_class.R


setGeneric(name = "create_ptf_bought_oblig", def = function(x, coefficient){standardGeneric("create_ptf_bought_oblig")})
setMethod(
    f = "create_ptf_bought_oblig",
    signature = c(x = "Oblig", coefficient = "numeric"),
    definition = function(x,coefficient){
        if (length(coefficient) != nrow(x@ptf_oblig)){stop("[Oblig : create_ptf_bought_oblig] : Les inputs sont de dimensions distinctes \n")}
        x@ptf_oblig$val_marche <- coefficient * x@ptf_oblig$val_marche
        x@ptf_oblig$val_nc     <- coefficient * x@ptf_oblig$val_nc
        x@ptf_oblig$val_achat  <- coefficient * x@ptf_oblig$val_achat
        x@ptf_oblig$nb_unit    <- coefficient * x@ptf_oblig$nb_unit
        x@ptf_oblig$cc         <- coefficient * x@ptf_oblig$cc
        x@ptf_oblig$sd         <- coefficient * x@ptf_oblig$sd
        # Il n'y a pas a ajuster les durations et Zsp car un mouvement parallele des flux n'impacte pas ces quantites

        # Validation de l'objet
        validObject(x)

        return(x)
    }
)
