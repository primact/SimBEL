#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe Immo
#--------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Classe pour les actifs de type immobilier.
##'
##' @name Immo
##' @slot ptf_immo est un dataframe, chaque ligne represente un actif immobilier du portefeuille d'immobilier.
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @export
##' @seealso Les operations d'achat vente immo  \code{\link{buy_immo}} et \code{\link{sell_immo}}.
setClass(
  Class = "Immo",
  representation = representation(
    ptf_immo = "data.frame")
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity ("Immo",
             function (object){
               retval <- NULL
               #Verification du nombre de colonnes
               if(dim(object@ptf_immo)[2]!=12) {retval <- c(retval, "[Immo] : Nombre d'attributs incorrect, un ptf Immo est compos? d'un DF de 12 colonnes /n")}

               # Verification du type des colonnes
               if (!is.integer(object@ptf_immo[,1]))  {retval <- c(retval, "[Immo] : num_mp n'est pas entier/n")}
               if (!is.numeric(object@ptf_immo[,2]))   {retval <- c(retval, "[Immo] : val_marche n'est pas reel/n")}
               if (!is.numeric(object@ptf_immo[,3]))   {retval <- c(retval, "[Immo] : val_nc n'est pas reel/n")}
               if (!is.numeric(object@ptf_immo[,4]))   {retval <- c(retval, "[Immo] : val_achat n'est pas reel/n")}
               if (!is.logical(object@ptf_immo[,5]))  {retval <- c(retval, "[Immo] : presence n'est pas logical/n")}
               if (!is.logical(object@ptf_immo[,6]))  {retval <- c(retval, "[Immo] : cessible n'est pas logical/n")}
               if (!is.numeric(object@ptf_immo[,7]))   {retval <- c(retval, "[Immo] : nb_unit n'est pas reel/n")}
               if (!is.numeric(object@ptf_immo[,8]))   {retval <- c(retval, "[Immo] : dur_det n'est pas reel/n")}
               if (!is.numeric(object@ptf_immo[,9]))   {retval <- c(retval, "[Immo] : pdd n'est pas reel/n")}
               if (!is.integer(object@ptf_immo[,10])) {retval <- c(retval, "[Immo] : num_index n'est pas integer/n")}
               if (!is.numeric(object@ptf_immo[,11]))  {retval <- c(retval, "[Immo] : loyer n'est pas reel/n")}
               if (!is.logical(object@ptf_immo[,12])) {retval <- c(retval, "[Immo] : ind_invest n'est pas logical/n")}

               # Verification du nom des colonnes
               if(sum(colnames(object@ptf_immo)==c("num_mp","val_marche","val_nc","val_achat",
                                                   "presence","cessible","nb_unit","dur_det",
                                                   "pdd","num_index","loyer","ind_invest"))!=12)
               {retval <- c(retval, "[Immo] : Noms de colonne incorrect/n")}

               if (is.null(retval)) return (TRUE)
               else return (retval)
             })

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement
setMethod(
  f = "initialize",
  signature = "Immo",
  definition = function(.Object, ptf=data.frame()){
    # Traitement du cas o? tout les ?l?ments sont renseign?s
    if( !missing(ptf)){
      if(ncol(ptf)!=12 | sum(names(ptf)!=c("num_mp","val_marche","val_nc","val_achat",
                                           "presence","cessible","nb_unit","dur_det",
                                           "pdd","num_index","loyer","ind_invest"))!=0)
      {stop("[Immo] : Nombre ou nommage des colonnes du dataframe incorrect")}
      else if(
        !is.integer(ptf[,"num_mp"])   | !is.numeric(ptf[,"val_marche"]) | !is.numeric(ptf[,"val_nc"])  | !is.numeric(ptf[,"val_achat"]) |
        !is.logical(ptf[,"presence"]) | !is.logical(ptf[,"cessible"])  | !is.numeric(ptf[,"nb_unit"]) | !is.numeric(ptf[,"dur_det"])   |
        !is.numeric(ptf[,"pdd"])       | !is.integer(ptf[,"num_index"]) | !is.numeric(ptf[,"loyer"])     | !is.logical(ptf[,"ind_invest"]))
      {stop("[Immo] : Typage incorrect des colonnes du dataframe")}
      else
      {.Object@ptf_immo <- ptf
      validObject(.Object)}
    }
    #Traitement du cas vide
    else
    {.Object@ptf_immo  <- data.frame(integer(),numeric(),numeric(),numeric(),logical(),logical(),numeric(),numeric(),numeric(),integer(),numeric(),logical())
    colnames(.Object@ptf_immo) <- c("num_mp","val_marche","val_nc","val_achat","presence","cessible","nb_unit","dur_det","pdd","num_index","loyer","ind_invest")
    }
    return(.Object)
  }
)
