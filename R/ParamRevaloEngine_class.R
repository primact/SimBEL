#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe ParamRevaloEngine
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Classe pour ParamRevaloEngine
##'
##' @name ParamRevaloEngine
##'
setClass(Class = "ParamRevaloEngine",
  representation = representation(
    taux_pb_fi = "numeric",
    taux_pb_tech = "numeric",
    tx_marge_min = "numeric",
    solde_pb_regl = "numeric"))

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity(Class = "ParamRevaloEngine",
            function (object){
              retval <- NULL
              if (!is.numeric(object@taux_pb_fi))  {retval <- c(retval, "[ParamRevaloEngine] : taux_pb_fi n'est pas numeric/n")}
              if (!is.numeric(object@taux_pb_tech))  {retval <- c(retval, "[ParamRevaloEngine] : taux_pb_tech n'est pas numeric/n")}
              if (!is.numeric(object@tx_marge_min))  {retval <- c(retval, "[ParamRevaloEngine] : tx_marge_min n'est pas numeric/n")}
              if (!is.numeric(object@solde_pb_regl))  {retval <- c(retval, "[ParamRevaloEngine] : solde_pb_regl n'est pas numeric/n")}

              if (object@solde_pb_regl > 0)  {retval <- c(retval, "[ParamRevaloEngine] : solde_pb_regl doit etre negatif ou nul/n")}

              if (is.null(retval)) return (TRUE)
              else return (retval)
            }
)

