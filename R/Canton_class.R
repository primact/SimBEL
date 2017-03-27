#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe Canton
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{Canton}.
##'
##' Une classe pour le canton d'un assureur. Un objet de cette classe agrege un portefeuille financier,
##' un portefeuille de passifs, l'ensemble des autres provisions ainsi que les parametres et donnees necessaires
##' a la projection de la situation d'un l'assureur.
##' @name Canton
##' @slot annee une valeur entiere correspondant a l'annee de projection.
##' @slot ptf_fin est un objet de type \code{\link{PortFin}},
##' qui represente le portefeuille d'investissement d'un canton.
##' @slot ptf_passif est un objet de type \code{\link{PortPassif}},
##' qui represente le portefeuille de passif d'un canton.
##' @slot mp_esg est un objet de type \code{\link{ModelPointESG}},
##' qui represente la situation courante deduite de l'ESG. Cet objet traduit la situation economique
##' pour une annee donnee et une simulation donnee.
##' @slot ppb est un objet de type \code{\link{Ppb}},
##'  qui represente la provision pour participation aux benefices (PPB).
##' @slot hyp_canton est un objet de type \code{\link{HypCanton}},
##'  qui regroupe les hypotheses generales applicables au canton.
##' @slot param_alm est un objet de type \code{\link{ParamAlmEngine}},
##'  qui contient les parametres utilises dans les methodes de gestion de l'allocation d'actifs.
##' @slot param_revalo est un objet de type \code{\link{ParamRevaloEngine}},
##'  qui contient les parametres utilises dans les methodes de gestion de la revalorisation.
##' @docType class
##' @author Prim'Act
##' @seealso La projection du \code{Canton} sur une annee : \code{\link{proj_an}}.
##' Le calcul du resultat technique : \code{\link{calc_result_technique_ap_pb}}.
##' Le calcul des fins de projection : \code{\link{calc_fin_proj}}.
##' @keywords classes
##' @export
##' @include PortFin_class.R PortPassif-class.R ModelPointESG_class.R Ppb_class.R HypCanton_class.R ParamAlmEngine_class.R ParamRevaloEngine_class.R
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
