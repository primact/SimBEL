#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe Canton
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{RevaloEngine}.
##'
##' Une classe comprenant les methodes pour l'application de la revalorisation des passifs.
##' @name RevaloEngine
##' @slot param_revalo est objet de type \code{\link{ParamRevaloEngine}} comprenant
##'  les parametres utilises pour la revalorisation des contrats.
##' @docType class
##' @author Prim'Act
##' @keywords classes
##' @include ParamRevaloEngine_class.R

setClass(
    Class = "RevaloEngine",
    representation = representation(
        param_revalo = "ParamRevaloEngine"
    )
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity(
    Class = "RevaloEngine",
    function(object) {
        retval <- NULL

        if (!validObject(object@param_revalo)) {
            retval <- "[RevaloEngine] : Objet ParamRevalo non valide"
        }


        if (is.null(retval)) {
            return(TRUE)
        } else {
            return(retval)
        }
    }
)
