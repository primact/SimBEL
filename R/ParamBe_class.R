#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe ParamBe
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{ParamBe}.
##'
##' Une classe contenant le nombre d'annees de projection utilise
##' pour le calcul du best estimate d'un assureur.
##' @name ParamBe
##' @slot nb_annee un entier comprenant le nombre d'annees de projection.
##' @docType class
##' @author Prim'Act
##' @keywords classes
##' @export
setClass(Class = "ParamBe",
         representation = representation(
             nb_annee = "integer"
         ))

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity(Class = "ParamBe",
            function (object){
                
                retval <- NULL
                
                # Test
                if (!is.integer(object@nb_annee))  retval <- c(retval, "[ParamBe] : nb_annee n'est pas entier/n")
                
                if (is.null(retval)) return (TRUE)
                else return (retval)
            }
)

