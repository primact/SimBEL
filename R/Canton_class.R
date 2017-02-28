#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe Canton
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe Canton
##'
##' Classe pour le canton d'un assureur
##'
##' @name Canton
##' @slot annee est une valeur \code{integer} correspondant a l'annee de projection.
##' @slot ptf_fin est un objet de type \code{PortFin}, qui represente le portefeuille d'investissement d'un canton.
##' @slot ptf_passif est un objet de type \code{PortPassif}, qui represente le portefeuille de passif d'un canton.
##' @slot mp_esg est un objet de type \code{ModelPointESG}, qui represente la situation courante
##' en annee et simulations des valeurs de l'ESG.
##' @slot ppb est un objet de type \code{Ppb}, qui represente la provision pour participation aux benefices.
##' @slot hyp_canton est un objet de type \code{HypCanton}, qui regroupe les hypotheses generales applicables au canton.
##' @slot param_alm est un objet de type \code{ParamAlmEngine}, qui contient les parametres
##'  utilises dans les methodes de gestion de l'allocation d'actifs.
##' @slot param_revalo est un objet de type \code{ParamRevaloEngine}, qui contient les parametres
##'  utilises dans les methodes de gestion de la revalorisation.
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export
setClass(
  Class = "Canton",
  representation = representation(
    annee = "integer",
    ptf_fin = "PortFin",
    ptf_passif = "PortPassif",
    mp_esg = "ModelPointESG",
    ppb = "Ppb",
    hyp_canton = "HypCanton",
    param_alm = "ParamAlmEngine",
    param_revalo = "ParamRevaloEngine"
    ),
  validity = function (object){
              retval <- NULL

              if (!is.integer(object@annee))  {retval <- c(retval, "[Canton] : annee n'est pas integer/n")}
              if(!validObject(object@ptf_fin)) {retval <- c("[Canton] : Objet PortFin non valide")}
              if(!validObject(object@ptf_passif))   {retval <- c("[Canton] : Objet PortPassif non valide")}
              if(!validObject(object@mp_esg))   {retval <- c("[Canton] : Objet MPESG non valide")}
              if(!validObject(object@ppb))   {retval <- c("[Canton] : Objet Ppb non valide")}
              if(!validObject(object@hyp_canton))   {retval <- c("[Canton] : Objet HypCanton non valide")}
              if(!validObject(object@param_alm))   {retval <- c("[Canton] : Objet ParamAlmEngine non valide")}
              if(!validObject(object@param_revalo))   {retval <- c("[Canton] : Objet ParamRevalo non valide")}

              if (is.null(retval)) return (TRUE)
              else return (retval)
              }
            )



# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement
setMethod(
  f = "initialize",
  signature = "Canton",
  definition = function(.Object,
                        annee = integer(),
                        ptf_fin = new("PortFin"),
                        ptf_passif = new("PortPassif"),
                        mp_esg = new("ModelPointESG"),
                        ppb = new("Ppb"),
                        hyp_canton = new("HypCanton"),
                        param_alm = new("ParamAlmEngine"),
                        param_revalo = new("ParamRevaloEngine")
  ){
    if(!missing(annee)      & !missing(ptf_fin) & ! missing(ptf_passif) & !missing(mp_esg)  & !missing(ppb) &
       !missing(hyp_canton) & !missing(param_alm)   & !missing(param_revalo)){
      .Object@annee      <- annee
      .Object@ptf_fin <- ptf_fin
      .Object@ptf_passif   <- ptf_passif
      .Object@mp_esg  <- mp_esg
      .Object@ppb  <- ppb
      .Object@hyp_canton <- hyp_canton
      .Object@param_alm  <- param_alm
      .Object@param_revalo  <- param_revalo
      validObject(.Object)
    } else {
      #Traitement du cas vide
      .Object@annee = as.integer(0)
      .Object@ptf_fin = new("PortFin")
      .Object@ptf_passif = new("PortPassif")
      .Object@mp_esg = new("ModelPointESG")
      .Object@ppb = new("Ppb")
      .Object@hyp_canton = new("HypCanton")
      .Object@param_alm = new("ParamAlmEngine")
      .Object@param_revalo = new("ParamRevaloEngine")
    }
    return(.Object)
  }
)
