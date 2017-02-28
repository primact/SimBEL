#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe Action
#--------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe Action
##'
##' Classe pour les actifs de type Action
##'
##' @name Action
##' @slot ptf_action est un dataframe, chaque ligne represente un actif action du portefeuille d'action.
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export
#removeClass("Action")
setClass(
  Class = "Action",
  representation = representation(
    ptf_action = "data.frame")
)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------

# Verificateur : permet ? chaque appel de l'objet de v?rifier quelques ?l?ments de base :
setValidity ("Action",
             function (object){
               retval <- NULL
               #Verification du nombre de colonnes
               if(dim(object@ptf_action)[2]!=12) {retval <- c(retval, "[Action] : Nombre d'attributs incorrect, un ptf Action est compos? d'un DF de 12 colonnes /n")}

               # Verification du type des colonnes
               if (!is.integer(object@ptf_action[,1]))  {retval <- c(retval, "[Action] : num_mp n'est pas entier/n")}
               if (!is.double(object@ptf_action[,2]))   {retval <- c(retval, "[Action] : val_marche n'est pas reel/n")}
               if (!is.double(object@ptf_action[,3]))   {retval <- c(retval, "[Action] : val_nc n'est pas reel/n")}
               if (!is.double(object@ptf_action[,4]))   {retval <- c(retval, "[Action] : val_achat n'est pas reel/n")}
               if (!is.logical(object@ptf_action[,5]))  {retval <- c(retval, "[Action] : presence n'est pas logical/n")}
               if (!is.logical(object@ptf_action[,6]))  {retval <- c(retval, "[Action] : cessible n'est pas logical/n")}
               if (!is.double(object@ptf_action[,7]))   {retval <- c(retval, "[Action] : nb_unit n'est pas reel/n")}
               if (!is.double(object@ptf_action[,8]))   {retval <- c(retval, "[Action] : dur_det n'est pas reel/n")}
               if (!is.double(object@ptf_action[,9]))   {retval <- c(retval, "[Action] : pdd n'est pas reel/n")}
               if (!is.integer(object@ptf_action[,10])) {retval <- c(retval, "[Action] : num_index n'est pas integer/n")}
               if (!is.double(object@ptf_action[,11]))  {retval <- c(retval, "[Action] : div n'est pas reel/n")}
               if (!is.logical(object@ptf_action[,12])) {retval <- c(retval, "[Action] : ind_invest n'est pas logical/n")}

               # Verification du nom des colonnes
               if(sum(colnames(object@ptf_action)==c("num_mp","val_marche","val_nc","val_achat",
                                                     "presence","cessible","nb_unit","dur_det",
                                                     "pdd","num_index","div","ind_invest"))!=12)
               {retval <- c(retval, "[Action] : Noms de colonne incorrect/n")}

               if (is.null(retval)) return (TRUE)
               else return (retval)
             })

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement
setMethod(
  f = "initialize",
  signature = "Action",
  definition = function(.Object, ptf=data.frame()){
    # Traitement du cas o? tout les ?l?ments sont renseign?s
    if( !missing(ptf)){
      if(ncol(ptf)!=12 | sum(names(ptf)!=c("num_mp","val_marche","val_nc","val_achat",
                                           "presence","cessible","nb_unit","dur_det",
                                           "pdd","num_index","div","ind_invest"))!=0)
      {stop("[Action] : Nombre ou nommage des colonnes du dataframe incorrect")}
      else if(
        !is.integer(ptf[,"num_mp"])   | !is.double(ptf[,"val_marche"]) | !is.double(ptf[,"val_nc"])  | !is.double(ptf[,"val_achat"]) |
        !is.logical(ptf[,"presence"]) | !is.logical(ptf[,"cessible"])  | !is.double(ptf[,"nb_unit"]) | !is.double(ptf[,"dur_det"])   |
        !is.double(ptf[,"pdd"])       | !is.integer(ptf[,"num_index"]) | !is.double(ptf[,"div"])     | !is.logical(ptf[,"ind_invest"]))
      {stop("[Action] : Typage incorrect des colonnes du dataframe")}
      else
      {.Object@ptf_action <- ptf
      validObject(.Object)}
    }
    #Traitement du cas vide
    else
    {.Object@ptf_action  <- data.frame(integer(),double(),double(),double(),logical(),logical(),double(),double(),double(),integer(),double(),logical())
    colnames(.Object@ptf_action) <- c("num_mp","val_marche","val_nc","val_achat","presence","cessible","nb_unit","dur_det","pdd","num_index","div","ind_invest")
    }
    return(.Object)
  }
)
