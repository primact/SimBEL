#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe Oblig
#--------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Classe pour les actifs de type obligation.
##'
##' @name Oblig
##' @slot ptf_oblig est un dataframe, chaque ligne represente un actif obligation du portefeuille d'obligation.
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @export
##' @seealso Les operations d'achat vente obligations  \code{\link{buy_oblig}} et \code{\link{sell_oblig}}.

setClass(
  Class = "Oblig",
  representation = representation(
    ptf_oblig = "data.frame"
  ))

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Verificateur et initialisateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
# Verificateur : permet a chaque appel de l'objet de verifier quelques elements de base :
setValidity(Class = "Oblig",
            function (object){
              retval <- NULL
              nb_col <- 18
              #Verification du nombre de colonnes
              if(dim(object@ptf_oblig)[2] != nb_col) {retval <- c(retval, "[Oblig] : Nombre d'attributs incorrect, un ptf Oblig est compos? d'un DF de 12 colonnes /n")}

              # Verification du type des colonnes
              if (!is.integer(object@ptf_oblig[,1]))  {retval <- c(retval, "[Oblig] : num_mp n'est pas entier/n")}
              if (!is.numeric(object@ptf_oblig[,2]))   {retval <- c(retval, "[Oblig] : val_marche n'est pas reel/n")}
              if (!is.numeric(object@ptf_oblig[,3]))   {retval <- c(retval, "[Oblig] : val_nc n'est pas reel/n")}
              if (!is.numeric(object@ptf_oblig[,4]))   {retval <- c(retval, "[Oblig] : val_achat n'est pas reel/n")}
              if (!is.logical(object@ptf_oblig[,5]))  {retval <- c(retval, "[Oblig] : presence n'est pas reel/n")}
              if (!is.logical(object@ptf_oblig[,6]))  {retval <- c(retval, "[Oblig] : cessible n'est pas reel/n")}
              if (!is.numeric(object@ptf_oblig[,7]))   {retval <- c(retval, "[Oblig] : nb_unit n'est pas reel/n")}
              if (!is.numeric(object@ptf_oblig[,8]))   {retval <- c(retval, "[Oblig] : dur_det n'est pas reel/n")}
              if (!is.numeric(object@ptf_oblig[,9]))   {retval <- c(retval, "[Oblig] : nominal n'est pas reel/n")}
              if (!is.numeric(object@ptf_oblig[,10]))  {retval <- c(retval, "[Oblig] : Taux coupon n'est pas reel/n")}
              if (!is.numeric(object@ptf_oblig[,11]))  {retval <- c(retval, "[Oblig] : Parite n'est pas reel/n")}
              if (!is.numeric(object@ptf_oblig[,12]))  {retval <- c(retval, "[Oblig] : Maturite residuelle n'est pas reel/n")}
              if (!is.factor(object@ptf_oblig[,13]))  {retval <- c(retval, "[Oblig] : Type n'est pas factor/n")}
              if (!is.integer(object@ptf_oblig[,14])) {retval <- c(retval, "[Oblig] : Rating n'est pas integer/n")}
              if (!is.numeric(object@ptf_oblig[,15]))  {retval <- c(retval, "[Oblig] : Duration n'est pas reel/n")}
              if (!is.numeric(object@ptf_oblig[,16]))  {retval <- c(retval, "[Oblig] : Zspread n'est pas reel/n")}
              if (!is.numeric(object@ptf_oblig[,17]))  {retval <- c(retval, "[Oblig] : Coupon couru n'est pas reel/n")}
              if (!is.numeric(object@ptf_oblig[,18]))  {retval <- c(retval, "[Oblig] : Surcote/Decote n'est pas reel/n")}

              # Verification du nom des colonnes
              if(sum(colnames(object@ptf_oblig)==c("num_mp","val_marche","val_nc","val_achat",
                                                   "presence","cessible","nb_unit","dur_det",
                                                   "nominal","tx_coupon","par","mat_res","type",
                                                   "rating","duration","zspread","cc","sd")      ) != nb_col)
              {retval <- c(retval, "[Oblig] : Noms de colonne incorrect/n")}

              if (is.null(retval)) return (TRUE)
              else return (retval)})

# Initialisateur : permet de construire l'objet initial, selon 2 cas :
#           - Objet vide
#           - Objet renseign?.
#           - Erreur autrement
setMethod(
  f = "initialize",
  signature = "Oblig",
  definition = function(.Object, ptf=data.frame()){
    nb_col <- 18
    # Traitement du cas o? tout les ?l?ments sont renseign?s
    if( !missing(ptf)){
      if(ncol(ptf) != nb_col | sum(names(ptf) != c("num_mp","val_marche","val_nc","val_achat",
                                                   "presence","cessible","nb_unit","dur_det",
                                                   "nominal","tx_coupon","par","mat_res","type",
                                                   "rating","duration","zspread","cc","sd")) != 0)
      {stop("[Oblig] : Nombre ou nommage des colonnes du dataframe incorrect")}
      else if(
        !is.integer(ptf[,"num_mp"])   | !is.numeric(ptf[,"val_marche"]) | !is.numeric(ptf[,"val_nc"])  | !is.numeric(ptf[,"val_achat"]) |
        !is.logical(ptf[,"presence"]) | !is.logical(ptf[,"cessible"])  | !is.numeric(ptf[,"nb_unit"]) | !is.numeric(ptf[,"dur_det"])   |
        !is.numeric(ptf[,"nominal"])   | !is.numeric(ptf[,"tx_coupon"])  | !is.numeric(ptf[,"par"])     | !is.numeric(ptf[,"mat_res"])   |
        !is.factor(ptf[,"type"])      | !is.integer(ptf[,"rating"])    | !is.numeric(ptf[,"duration"])| !is.numeric(ptf[,"zspread"])   |
        !is.numeric(ptf[,"cc"])        | !is.numeric(ptf[,"sd"]))  {stop("[Oblig] : Typage incorrect des colonnes du dataframe")}
      else
      {.Object@ptf_oblig <- ptf
      validObject(.Object)}
    }
    #Traitement du cas vide
    else
    {.Object@ptf_oblig  <- data.frame(integer(),numeric(),numeric(),numeric(),logical(),logical(),numeric(),numeric(),numeric(),numeric(),numeric(),numeric(),factor(),integer(),numeric(),numeric(),numeric(),numeric())
    colnames(.Object@ptf_oblig) <- c("num_mp","val_marche","val_nc","val_achat","presence","cessible","nb_unit","dur_det","nominal","tx_coupon","par","mat_res","type","rating","duration","zspread","cc","sd")
    }
    return(.Object)
  }
)
