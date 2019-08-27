#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe ParamChocSousc
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{ParamChocSousc}.
##'
##' Une classe contenant les parametres des chocs souscription de la formule standard.
##' @name ParamChocSousc
##' @slot mp un \code{data.frame} contenant l'ensemble des parametres necessaires a l'application des
##'  chocs du module Souscription Vie.
##' @docType class
##' @author Prim'Act
##' @keywords classes
##' @export
setClass(
  Class = "ParamChocSousc",
  slots = c(mp = "data.frame"),
  validity = function (object){
    # liste permettant de stocker les erreurs de chargement
    retval <- NULL
    nb_col_attendu <- 9
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
    if (!is.numeric(object@mp[,9]))   {retval <- c(retval, "[ParamChoc] : choc_rachat_massif n'est pas numerique\n")}


    # Verification du nom des colonnes
    if(sum(colnames(object@mp) == c("choc_frais_inflation","choc_frais_assiette","choc_mortalite",
                                    "choc_longevite","choc_rachat_up","choc_rachat_up_lim","choc_rachat_down",
                                    "choc_rachat_down_lim", "choc_rachat_massif")) != nb_col_attendu){
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
