

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Classe pour les actifs de type obligation.
##'
##' @name Oblig
##' @slot ptf_oblig est un dataframe, chaque ligne represente un actif obligation du portefeuille d'obligation.
##' @docType class
##' @keywords classes
##' @section Lien a creer
##' @author Prim'Act
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
              if (!is.integer(.subset2(object@ptf_oblig,1)))   {retval <- c(retval, "[Oblig] : num_mp n'est pas entier/n")}
              if (!is.numeric(.subset2(object@ptf_oblig,2)))   {retval <- c(retval, "[Oblig] : val_marche n'est pas reel/n")}
              if (!is.numeric(.subset2(object@ptf_oblig,3)))   {retval <- c(retval, "[Oblig] : val_nc n'est pas reel/n")}
              if (!is.numeric(.subset2(object@ptf_oblig,4)))   {retval <- c(retval, "[Oblig] : val_achat n'est pas reel/n")}
              if (!is.logical(.subset2(object@ptf_oblig,5)))   {retval <- c(retval, "[Oblig] : presence n'est pas reel/n")}
              if (!is.logical(.subset2(object@ptf_oblig,6)))   {retval <- c(retval, "[Oblig] : cessible n'est pas reel/n")}
              if (!is.numeric(.subset2(object@ptf_oblig,7)))   {retval <- c(retval, "[Oblig] : nb_unit n'est pas reel/n")}
              if (!is.numeric(.subset2(object@ptf_oblig,8)))   {retval <- c(retval, "[Oblig] : dur_det n'est pas reel/n")}
              if (!is.numeric(.subset2(object@ptf_oblig,9)))   {retval <- c(retval, "[Oblig] : nominal n'est pas reel/n")}
              if (!is.numeric(.subset2(object@ptf_oblig,10)))  {retval <- c(retval, "[Oblig] : Taux coupon n'est pas reel/n")}
              if (!is.numeric(.subset2(object@ptf_oblig,11)))  {retval <- c(retval, "[Oblig] : Parite n'est pas reel/n")}
              if (!is.numeric(.subset2(object@ptf_oblig,12)))  {retval <- c(retval, "[Oblig] : Maturite residuelle n'est pas reel/n")}
              if (!is.factor(.subset2(object@ptf_oblig,13)))   {retval <- c(retval, "[Oblig] : Type n'est pas factor/n")}
              if (!is.integer(.subset2(object@ptf_oblig,14)))  {retval <- c(retval, "[Oblig] : Rating n'est pas integer/n")}
              if (!is.numeric(.subset2(object@ptf_oblig,15)))  {retval <- c(retval, "[Oblig] : Duration n'est pas reel/n")}
              if (!is.numeric(.subset2(object@ptf_oblig,16)))  {retval <- c(retval, "[Oblig] : Zspread n'est pas reel/n")}
              if (!is.numeric(.subset2(object@ptf_oblig,17)))  {retval <- c(retval, "[Oblig] : Coupon couru n'est pas reel/n")}
              if (!is.numeric(.subset2(object@ptf_oblig,18)))  {retval <- c(retval, "[Oblig] : Surcote/Decote n'est pas reel/n")}

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
        nom_table   <- names(ptf)
        num_mp      <- which(nom_table == "num_mp")
        val_marche  <- which(nom_table == "val_marche")
        val_nc      <- which(nom_table == "val_nc")
        val_achat   <- which(nom_table == "val_achat")
        presence    <- which(nom_table == "presence")
        cessible    <- which(nom_table == "cessible")
        nb_unit     <- which(nom_table == "nb_unit")
        dur_det     <- which(nom_table == "dur_det")
        nominal     <- which(nom_table == "nominal")
        tx_coupon   <- which(nom_table == "tx_coupon")
        par         <- which(nom_table == "par")
        mat_res     <- which(nom_table == "mat_res")
        type        <- which(nom_table == "type")
        rating      <- which(nom_table == "rating")
        duration    <- which(nom_table == "duration")
        zspread     <- which(nom_table == "zspread")
        cc          <- which(nom_table == "cc")
        sd          <- which(nom_table == "sd")

        if(ncol(ptf) != nb_col | sum(names(ptf) != c("num_mp","val_marche","val_nc","val_achat",
                                                   "presence","cessible","nb_unit","dur_det",
                                                   "nominal","tx_coupon","par","mat_res","type",
                                                   "rating","duration","zspread","cc","sd")) != 0)
      {stop("[Oblig] : Nombre ou nommage des colonnes du dataframe incorrect")}
      else if(
        !is.integer(.subset2(ptf, num_mp))   | !is.numeric(.subset2(ptf, val_marche)) | !is.numeric(.subset2(ptf,val_nc))  | !is.numeric(.subset2(ptf, val_achat)) |
        !is.logical(.subset2(ptf, presence)) | !is.logical(.subset2(ptf, cessible))   | !is.numeric(.subset2(ptf,nb_unit)) | !is.numeric(.subset2(ptf, dur_det))   |
        !is.numeric(.subset2(ptf, nominal))  | !is.numeric(.subset2(ptf, tx_coupon))  | !is.numeric(.subset2(ptf,par))     | !is.numeric(.subset2(ptf, mat_res))   |
        !is.factor(.subset2(ptf, type))      | !is.integer(.subset2(ptf, rating))     | !is.numeric(.subset2(ptf,duration))| !is.numeric(.subset2(ptf, zspread))   |
        !is.numeric(.subset2(ptf, cc))       | !is.numeric(.subset2(ptf, sd)))  {stop("[Oblig] : Typage incorrect des colonnes du dataframe")}
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
