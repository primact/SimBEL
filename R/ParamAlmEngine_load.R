#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de chargement des hyp des hypotheses ALM
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Chargement des attributs d'un objet \code{ParamAlmEngine} a partir des donnees utilisateurs.
##'
##' \code{param_alm_engine_load} est la methode de chargement des attributs d'un objet \code{\link{ParamAlmEngine}}
##' a partir des donnees de l'environnement utilisateur et d'un portefeuille financier de reference (charge par la fonction \code{\link{chargement_PortFin_ref}}.
##' @name param_alm_engine_load
##' @docType methods
##' @param file_alm_address un \code{character} contenant l'adresse exacte
##' du fichier d'input utilisateur.
##' @param ptf_fin_ref un objet de la classe \code{\link{PortFin}} correspondant au portefeuille de reinvestissement.
##' @return L'objet de la classe \code{\link{ParamAlmEngine}} construit a partir des inputs renseignes par l'utilisateur.
##' @author Prim'Act
##' @export
##' @include ParamAlmEngine_class.R PortFin_class.R

setGeneric(name = "param_alm_engine_load", def = function(file_alm_address, ptf_fin_ref){standardGeneric("param_alm_engine_load")})
setMethod(
    f = "param_alm_engine_load",
    signature = c("character","PortFin"),
    definition = function(file_alm_address, ptf_fin_ref){
        temp          <- read.csv2(file_alm_address)
        param_alm     <- new("ParamAlmEngine",
                             ptf_reference = ptf_fin_ref,
                             alloc_cible   = c(temp[,"alloc_action"],
                                               temp[,"alloc_immo"],
                                               temp[,"alloc_oblig"],
                                               temp[,"alloc_treso"]),
                             seuil_realisation_PVL  = temp[,"seuil_realisation_PVL"])
        return(param_alm)
    }
)
