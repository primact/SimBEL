#----------------------------------------------------------------------------------------------------------------------------------------------------
#           create_ptf_bought_action
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Ajuste les quantites d'actions a acheter.
##'
##' \code{create_ptf_bought_action} est une methode permettant d'ajuster d'un coefficient les quantites
##' d'actions a acheter. Cette methode est utilisee pour l'achat de nouvelles actions.
##' @name create_ptf_bought_action
##' @docType methods
##' @param x objet de la classe \code{\link{Action}}, correspondant au portefeuille actions de reinvestissement.
##' Ce portefeuille est unitaire.
##' @param coefficient un vecteur de type \code{numeric} qui a autant d'elements
##'  que le portefeuille de reinvestissement action a de lignes.
##'   Il correspond au coefficient a appliquer au portefeuille de reinvestissement action
##'   pour effectuer l'achat desire.
##' @return \code{x} un objet de la classe \code{\link{Action}} correspondant a une
##'  proportion precise du portefeuille de reinvestissement action.
##' @author Prim'Act
##' @seealso La classe \code{\link{Action}}.
##' @export
##' @aliases AlmEngine
##' @include AlmEngine_class.R Action_class.R

setGeneric(name = "create_ptf_bought_action", def = function(x, coefficient){standardGeneric("create_ptf_bought_action")})
setMethod(
    f = "create_ptf_bought_action",
    signature = c(x = "Action", coefficient = "numeric"),
    definition = function(x, coefficient){
        if (length(coefficient) != nrow(x@ptf_action)){
          stop("[Action : create_ptf_bought_action] : Les inputs sont de dimensions distinctes \n")
          }
        x@ptf_action$val_marche <- coefficient * x@ptf_action$val_marche
        x@ptf_action$val_nc     <- coefficient * x@ptf_action$val_nc
        x@ptf_action$val_achat  <- coefficient * x@ptf_action$val_achat
        x@ptf_action$nb_unit    <- coefficient * x@ptf_action$nb_unit
        return(x)
    }
)
