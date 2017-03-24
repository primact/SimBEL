#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe RevaloEngine
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe RevaloEngine
##'
##' Classe comprenant les methodes pour l'application de la revalorisation des passifs
##'
##' @name RevaloEngine
##' @slot param_revalo est objet de type \code{ParamRevalo} comprenant les parametres utilises pour la revalorisation des contrats.
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
setClass(
  Class = "RevaloEngine",
  representation = representation(
    param_revalo = "ParamRevaloEngine"))

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity(Class = "RevaloEngine",
            function (object){
              retval <- NULL

              if(!validObject(object@param_revalo))  {retval <- "[RevaloEngine] : Objet ParamRevalo non valide"}


              if (is.null(retval)) return (TRUE)
              else return (retval)
            }
)

