#---------------------------------------------------------------------------------------------------------
# Ce script est la definition de la classe ParamChoc qui contient les données sur parametres de choc
#---------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 10/02/2017. Fait par MT : initialisation
#----------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe ParamChoc
#----------------------------------------------------------------------------------------------------------------------------------------------------


##' La classe ParamChocSousc
##'
##' Classe pour pour les parametres de comportement
##'
##' @name ParamChocSousc
##' @slot mp : model point contenant l'ensemble des parametres pour faire les differents chocs
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export


setClass(
  Class = "ParamChocSousc",
  slots = c(mp = "data.frame"),
  validity = function (object){
    # liste permettant de stocker les erreurs de chargement
    retval <- NULL
    nb_col_attendu <- 8
    # Verification du nombre de colonnes
    if(ncol(object@mp) != nb_col_attendu){
      retval <- c(retval, "[ParamChoc] : Nombre de parametres incorrect\n")
    }
    
    # Verification du type des colonnes
    if (!is.numeric(object@mp[,1]))   {retval <- c(retval, "[ParamChoc] : choc_frais_inflation n'est pas numerique\n")}
    if (!is.numeric(object@mp[,2]))   {retval <- c(retval, "[ParamChoc] : choc_frais_assiette n'est pas numerique\n")}
    if (!is.numeric(object@mp[,3]))   {retval <- c(retval, "[ParamChoc] : choc_mortalite n'est pas numerique\n")}
    if (!is.numeric(object@mp[,4]))   {retval <- c(retval, "[ParamChoc] : choc_longevite n'est pas numerique\n")}
    if (!is.numeric(object@mp[,5]))   {retval <- c(retval, "[ParamChoc] : choc_rachat_up n'est pas numerique\n")}
    if (!is.numeric(object@mp[,6]))   {retval <- c(retval, "[ParamChoc] : choc_rachat_up_lim n'est pas numerique\n")}
    if (!is.numeric(object@mp[,7]))   {retval <- c(retval, "[ParamChoc] : choc_rachat_down n'est pas numerique\n")}
    if (!is.numeric(object@mp[,8]))   {retval <- c(retval, "[ParamChoc] : choc_rachat_down_lim n'est pas numerique\n")}
    
    
    # Verification du nom des colonnes
    if(sum(colnames(object@mp) == c("choc_frais_inflation","choc_frais_assiette","choc_mortalite",
                                    "choc_longevite","choc_rachat_up","choc_rachat_up_lim","choc_rachat_down",
                                    "choc_rachat_down_lim")) != nb_col_attendu){
      retval <- c(retval, "[ParamChoc] : Noms de colonne incorrect \n")
    }
    
    # Resultats du controle
    if (is.null(retval)){
      return (TRUE)
    }else{
      return (retval)
    }
  }
)
