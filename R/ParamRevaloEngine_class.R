#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe ParamRevaloEngine
#----------------------------------------------------------------------------------------------------------------------------------------------------
##'  La classe \code{ParamRevaloEngine}.
##
##' Une Classe pour les parametres utilises pour la gestion de la revalorisation.
##' @name ParamRevaloEngine
##' @slot taux_pb_fi une valeur \code{numeric} correspondant au taux de participation applique au resultat financier.
##' @slot taux_pb_tech une valeur \code{numeric} correspondant au taux de participation applique au resultat technique.
##' @slot tx_marge_min une valeur \code{numeric} correspondant au taux de marge minimal auquel s'attend l'assureur.
##' @slot solde_pb_regl une valeur \code{numeric} correspondant au solde deficitaire de participation aux
##' benefices reglementaire. Cette valeur doit etre negative.
##' @docType class
##' @author Prim'Act
##' @keywords classes
##' @export
setClass(Class = "ParamRevaloEngine",
         representation = representation(
             taux_pb_fi = "numeric",
             taux_pb_tech = "numeric",
             tx_marge_min = "numeric",
             solde_pb_regl = "numeric")
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity(Class = "ParamRevaloEngine",
            function (object){
                
                retval <- NULL
                
                if (!is.numeric(object@taux_pb_fi))    retval <- c(retval, "[ParamRevaloEngine] : taux_pb_fi n'est pas numeric/n")
                if (!is.numeric(object@taux_pb_tech))  retval <- c(retval, "[ParamRevaloEngine] : taux_pb_tech n'est pas numeric/n")
                if (!is.numeric(object@tx_marge_min))  retval <- c(retval, "[ParamRevaloEngine] : tx_marge_min n'est pas numeric/n")
                if (!is.numeric(object@solde_pb_regl)) retval <- c(retval, "[ParamRevaloEngine] : solde_pb_regl n'est pas numeric/n")
                
                if (object@solde_pb_regl > 0)  retval <- c(retval, "[ParamRevaloEngine] : solde_pb_regl doit etre negatif ou nul/n")
                
                if (is.null(retval)) 
                    return (TRUE)
                else 
                    return (retval)
            }
)

