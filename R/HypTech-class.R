#---------------------------------------------------------------------------------------------------------
# Ce script est la definition de la classe HypTech qui contient les hypotheses techniques chargees, i.e.
# les tables de mortalite, les tables de rachat et les parametres de rachats dynamiques
#---------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 21/01/2017. Fait par QG : initialisation
#----------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe HypTech
#----------------------------------------------------------------------------------------------------------------------------------------------------


##' La classe HypTech
##'
##' Classe pour pour les tables de rachat
##'
##' @name HypTech
##' @slot 4 listes contenant les data frame relatifs aux tables de mortalite, tables de rachat et parametres de rachat conjoncturels
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
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



