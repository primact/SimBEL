#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe HypTech
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{HypTech}.
##'
##' Une classe contenant les listes de tables de mortalite, de rachat, les parametres de rachat dynamique et
##' les parametres comportementaux qui permettent
##' de calculer les attentes en matiere de taux de revalorisation cible.
##' @name HypTech
##' @slot tables_mort une liste contenant des tables de mortalite au format \code{\link{ParamTableMort}}.
##' @slot tables_rach une liste contenant des tables de rachat (structurel) au format \code{\link{ParamTableRach}}.
##' @slot param_rach_dyn une liste contenant des parametres de rachat dynamique
##' au format \code{\link{ParamRachDyn}}.
##' @slot param_comport une liste contenant des des parametres comportementaux au format \code{\link{ParamComport}}.
##' @docType class
##' @details Chaque elements de ces liste doit avoir prealablement ete nomme.
##' @author Prim'Act
##' @seealso Les classes de parametres contenues \code{\link{ParamTableMort}}, \code{\link{ParamTableRach}},
##' \code{\link{ParamRachDyn}}, \code{\link{ParamComport}}.
##' La methode pour l'application des chocs de mortalite et de longevite : \code{\link{get_choc_table}}.
##' La methode pour l'application des chocs de rachat haut et bas : \code{\link{get_choc_rach}}.
##' La methode pour la recuperation des parametres comportementaux : \code{\link{get_comport}}.
##' La methode pour la recuperation des taux de deces : \code{\link{get_qx_mort}}.
##' La methode pour la recuperation des taux de rachat structurel : \code{\link{get_qx_rach}}.
##' La methode pour la recuperation des taux de rachat dynamique : \code{\link{get_rach_dyn}}.
##' @keywords classes
##' @export
setClass(
  Class = "HypTech",
  slots = c(tables_mort = "list",
            tables_rach = "list",
            param_rach_dyn = "list",
            param_comport = "list"),
  validity = function (object){

    # liste permettant de stocker les erreurs
    retval <- NULL

    # Test que les listes ne sont pas vides
    if(length(object@tables_mort) == 0){c(retval, "[HypTech] : 'tables_mort' ne doit pas etre vide \n")}
    if(length(object@tables_rach) == 0){c(retval, "[HypTech] : 'tables_rach' ne doit pas etre vide \n")}
    if(length(object@param_rach_dyn) == 0){c(retval, "[HypTech] : 'param_rach_dyn' ne doit pas etre vide \n")}
    if(length(object@param_comport) == 0){c(retval, "[HypTech] : 'param_comport' ne doit pas etre vide \n")}

    # Tests de non absence de noms
    if(is.null(names(object@tables_mort))){retval <- c(retval, "[HypTech] : 'tables_mort' doit etre une liste avec des noms \n")}
    if(is.null(names(object@tables_rach))){retval <- c(retval, "[HypTech] : 'tables_rach' doit etre une liste avec des noms \n")}
    if(is.null(names(object@param_rach_dyn))){retval <- c(retval, "[HypTech] : 'param_rach_dyn' doit etre une liste avec des noms \n")}
    if(is.null(names(object@param_comport))){retval <- c(retval, "[HypTech] : 'param_comport' doit etre une liste avec des noms \n")}

    # Test si un nom n a pas ete oublie
    if(! prod(names(object@tables_mort) != c("") )){
      retval <- c(retval, "[HypTech] : tous les elements de 'tables_mort' n ont pas ete nommes \n")}
    if(! prod(names(object@tables_rach) != c("") )){
      retval <- c(retval, "[HypTech] : tous les elements de 'tables_rach' n ont pas ete nommes \n")}
    if(! prod(names(object@param_rach_dyn) != c("") )){
      retval <- c(retval, "[HypTech] : tous les elements de 'param_rach_dyn' n ont pas ete nommes \n")}
    if(! prod(names(object@param_comport) != c("") )){
      retval <- c(retval, "[HypTech] : tous les elements de 'param_comport' n ont pas ete nommes \n")}

    # Test que les elements des listes sont bien des objets du bon type
    for(i in 1:length(object@tables_mort)){
      if(class(object@tables_mort[[i]]) != "ParamTableMort"){
        retval <- c(retval, paste("[HypTech] : l element ", i, " de 'tables_mort' n est pas de type 'ParamTableMort' \n", sep = ""))}
    }

    for(i in 1:length(object@tables_rach)){
      if(class(object@tables_rach[[i]]) != "ParamTableRach"){
        retval <- c(retval, paste("[HypTech] : l element ", i, " de 'tables_rach' n est pas de type 'ParamTableRach' \n", sep = ""))}
    }

    for(i in 1:length(object@param_rach_dyn)){
      if(class(object@param_rach_dyn[[i]]) != "ParamRachDyn"){
        retval <- c(retval, paste("[HypTech] : l element ", i, " de 'tables_rach_dyn' n est pas de type 'ParamRachDyn' \n", sep = ""))}
    }

    for(i in 1:length(object@param_comport)){
      if(class(object@param_comport[[i]]) != "ParamComport"){
        retval <- c(retval, paste("[HypTech] : l element ", i, " de 'param_comport' n est pas de type 'ParamComport' \n", sep = ""))}
    }

    # Resultats du controle
    if (is.null(retval)){
      return (TRUE)
    }else{
      return (cat(retval))
    }
  }
)



