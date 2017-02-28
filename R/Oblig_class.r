#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe Oblig
#--------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 23/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe Oblig
##'
##' Classe pour les actifs de type Obligation
##'
##' @name Oblig
##' @slot ptf_oblig est un dataframe, chaque ligne represente un actif obligataire du portefeuille d'obligation.
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export
#removeClass("Oblig")
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
              if (!is.double(object@ptf_oblig[,2]))   {retval <- c(retval, "[Oblig] : val_marche n'est pas reel/n")}
              if (!is.double(object@ptf_oblig[,3]))   {retval <- c(retval, "[Oblig] : val_nc n'est pas reel/n")}
              if (!is.double(object@ptf_oblig[,4]))   {retval <- c(retval, "[Oblig] : val_achat n'est pas reel/n")}
              if (!is.logical(object@ptf_oblig[,5]))  {retval <- c(retval, "[Oblig] : presence n'est pas reel/n")}
              if (!is.logical(object@ptf_oblig[,6]))  {retval <- c(retval, "[Oblig] : cessible n'est pas reel/n")}
              if (!is.double(object@ptf_oblig[,7]))   {retval <- c(retval, "[Oblig] : nb_unit n'est pas reel/n")}
              if (!is.double(object@ptf_oblig[,8]))   {retval <- c(retval, "[Oblig] : dur_det n'est pas reel/n")}
              if (!is.double(object@ptf_oblig[,9]))   {retval <- c(retval, "[Oblig] : nominal n'est pas reel/n")}
              if (!is.double(object@ptf_oblig[,10]))  {retval <- c(retval, "[Oblig] : Taux coupon n'est pas reel/n")}
              if (!is.double(object@ptf_oblig[,11]))  {retval <- c(retval, "[Oblig] : Parite n'est pas reel/n")}
              if (!is.double(object@ptf_oblig[,12]))  {retval <- c(retval, "[Oblig] : Maturite residuelle n'est pas reel/n")}
              if (!is.factor(object@ptf_oblig[,13]))  {retval <- c(retval, "[Oblig] : Type n'est pas factor/n")}
              if (!is.integer(object@ptf_oblig[,14])) {retval <- c(retval, "[Oblig] : Rating n'est pas integer/n")}
              if (!is.double(object@ptf_oblig[,15]))  {retval <- c(retval, "[Oblig] : Duration n'est pas reel/n")}
              if (!is.double(object@ptf_oblig[,16]))  {retval <- c(retval, "[Oblig] : Zspread n'est pas reel/n")}
              if (!is.double(object@ptf_oblig[,17]))  {retval <- c(retval, "[Oblig] : Coupon couru n'est pas reel/n")}
              if (!is.double(object@ptf_oblig[,18]))  {retval <- c(retval, "[Oblig] : Surcote/Decote n'est pas reel/n")}

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
        !is.integer(ptf[,"num_mp"])   | !is.double(ptf[,"val_marche"]) | !is.double(ptf[,"val_nc"])  | !is.double(ptf[,"val_achat"]) |
        !is.logical(ptf[,"presence"]) | !is.logical(ptf[,"cessible"])  | !is.double(ptf[,"nb_unit"]) | !is.double(ptf[,"dur_det"])   |
        !is.double(ptf[,"nominal"])   | !is.double(ptf[,"tx_coupon"])  | !is.double(ptf[,"par"])     | !is.double(ptf[,"mat_res"])   |
        !is.factor(ptf[,"type"])      | !is.integer(ptf[,"rating"])    | !is.double(ptf[,"duration"])| !is.double(ptf[,"zspread"])   |
        !is.double(ptf[,"cc"])        | !is.double(ptf[,"sd"]))  {stop("[Oblig] : Typage incorrect des colonnes du dataframe")}
      else
      {.Object@ptf_oblig <- ptf
      validObject(.Object)}
    }
    #Traitement du cas vide
    else
    {.Object@ptf_oblig  <- data.frame(integer(),double(),double(),double(),logical(),logical(),double(),double(),double(),double(),double(),double(),factor(),integer(),double(),double(),double(),double())
    colnames(.Object@ptf_oblig) <- c("num_mp","val_marche","val_nc","val_achat","presence","cessible","nb_unit","dur_det","nominal","tx_coupon","par","mat_res","type","rating","duration","zspread","cc","sd")
    }
    return(.Object)
  }
)
