#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe ParamBe
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Classe pour ParamBe
##'
##' @name ParamBe
##'
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
              if (!is.integer(object@nb_annee))  {retval <- c(retval, "[ParamBe] : nb_annee n'est pas entier/n")}
              if (is.null(retval)) return (TRUE)
              else return (retval)
            }
)

